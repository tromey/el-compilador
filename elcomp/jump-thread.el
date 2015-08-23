;;; Jump-threading pass. -*- lexical-binding:t -*-

;;; Commentary:

;; This implements a simple jump-threading pass.  See the doc string
;; of elcomp--thread-jumps-pass for details.

;;; Code:

(require 'elcomp)
(require 'elcomp/back)
(require 'elcomp/linearize)

(defun elcomp--get-not-argument (insn)
  "Check if INSN uses a 'not' condition.

INSN is an 'if' instruction.  If the condition was defined by a
call to 'not' (or 'null'), return the argument to the 'not'.
Otherwise return nil."
  (let ((call (oref insn :sym)))
    (if (and (elcomp--call-p call)
	     (memq (oref call :func) '(not null)))
	(car (oref call :args)))))

(defun elcomp--constant-nil-p (cst)
  "Return t if CST is an `elcomp--constant' whose value is nil."
  (and (elcomp--constant-p cst)
       (eq (oref cst :value) nil)))

(defun elcomp--get-eq-argument (insn)
  "Check if INSN uses an `eq' condition.

INSN is an `if' instruction.  If the condition is of the
form `(eq V nil)' or `(eq nil V)', return V.  Otherwise return
nil."
  (cl-assert (elcomp--if-p insn))
  (let ((call (oref insn :sym)))
    (if (and (elcomp--call-p call)
	     (memq (oref call :func) '(eq equal)))
	(let ((args (oref call :args)))
	  (cond
	   ((elcomp--constant-nil-p (car args))
	    (cadr args))
	   ((elcomp--constant-nil-p (cadr args))
	    (car args))
	   (t nil))))))

(defun elcomp--peel-condition (insn)
  "Peel `not' and other obviously unnecessary calls from INSN.
INSN is the variable used by an `if'."
  (let ((changed-one t))
    (while changed-one
      (setf changed-one nil)
      ;; Peel a 'not'.
      (let ((arg-to-not (elcomp--get-not-argument insn)))
	(when arg-to-not
	  (setf changed-one t)
	  (cl-rotatef (oref insn :block-true)
		      (oref insn :block-false))
	  (setf (oref insn :sym) arg-to-not)))
      ;; Change (eq V nil) or (eq nil V) to plain V.
      (let ((arg-to-eq (elcomp--get-eq-argument insn)))
	(when arg-to-eq
	  (setf changed-one t)
	  (setf (oref insn :sym) arg-to-eq))))))

(defun elcomp--block-has-catch (block tag)
  "If the block has a `catch' exception handler, return it.
Otherwise return nil.
TAG is a constant that must be matched by the handler."
  (catch 'done
    (dolist (exception (elcomp--basic-block-exceptions block))
      (cond
       ((elcomp--catch-p exception)
	(if (elcomp--constant-p (oref exception :tag))
	    (if (equal tag (oref exception :tag))
		(throw 'done exception)
	      ;; The tag is a different constant, so we can ignore
	      ;; this one and keep going.
	      nil)
	  ;; Non-constant tag could match anything.
	  (throw 'done nil)))
       ((elcomp--fake-unwind-protect-p exception)
	;; Keep going; we can handle these properly.
	)
       ((elcomp--condition-case-p exception)
	;; Keep going; we can ignore these.
	)
       ;; This requires re-linearizing the unwind-protect
       ;; original-form.  However we can't do this at present because
       ;; we've already lost information about the variable
       ;; remappings.  Perhaps it would be simpler to just go directly
       ;; into SSA when linearizing?
       ;; ((elcomp--unwind-protect-p exception)
       ;; 	;; Keep going.
       ;; 	)
       (t
	(throw 'done nil))))))

(defun elcomp--get-catch-symbol (exception)
  "Given a `catch' exception object, return the symbol holding the `throw' value."
  (cl-assert (elcomp--catch-p exception))
  (let ((insn (car (elcomp--basic-block-code (oref exception :handler)))))
    (cl-assert (elcomp--call-p insn))
    (cl-assert (eq (oref insn :func) :catch-value))
    (oref insn :sym)))

(defun elcomp--get-catch-target (exception)
  "Given a `catch' exception object, return the basic block of the `catch' itself."
  (cl-assert (elcomp--catch-p exception))
  (let ((insn (cadr (elcomp--basic-block-code (oref exception :handler)))))
    (cl-assert (elcomp--goto-p insn))
    (oref insn :block)))

(defun elcomp--maybe-replace-catch (block insn)
  ;; A `throw' with a constant tag can be transformed into an
  ;; assignment and a GOTO when the current block's outermost handler
  ;; is a `catch' of the same tag.
  (when (and (elcomp--diediedie-p insn)
	     (eq (oref insn :func) 'throw)
	     ;; Argument to throw is a const.
	     (elcomp--constant-p
	      (car (oref insn :args))))
    (let ((exception (elcomp--block-has-catch block
					      (car (oref insn :args)))))
      (when exception
	;; Whew.  First drop the last instruction from the block.
	(setf (elcomp--basic-block-code block)
	      (nbutlast (elcomp--basic-block-code block) 1))
	(setf (elcomp--basic-block-code-link block)
	      (last (elcomp--basic-block-code block)))
	;; Emit `unbind' calls.  (Note that when we can handle real
	;; unwind-protects we will re-linearize those here as well.)
	(let ((iter (elcomp--basic-block-exceptions block)))
	  (while (not (elcomp--catch-p (car iter)))
	    (when (elcomp--fake-unwind-protect-p (car iter))
	      (elcomp--add-to-basic-block
	       block
	       (elcomp--call :sym nil
			     :func :elcomp-unbind
			     :args (list
				    (elcomp--constant :value
						      (oref (car iter)
							    :count))))))
	    (setf iter (cdr iter))))
	;; Now add an instruction with an assignment and a goto, and
	;; zap the `diediedie' instruction.
	(elcomp--add-to-basic-block
	 block
	 (elcomp--set :sym (elcomp--get-catch-symbol exception)
		      :value (cadr (oref insn :args))))
	(elcomp--add-to-basic-block
	 block
	 (elcomp--goto :block (elcomp--get-catch-target exception)))
	t))))

(defun elcomp--thread-jumps-pass (compiler in-ssa-form)
  "A pass to perform jump threading on COMPILER.

This pass simplifies the CFG by eliminating redundant jumps.  In
particular, it:

* Converts redundant gotos like
       GOTO A;
    A: GOTO B;
  =>
       GOTO B;

* Likewise for either branch of an IF:
        IF E A; else B;
     A: GOTO C;
  =>
        IF E C; else B;

* Converts a redundant IF into a GOTO:
        IF E A; else A;
  =>
        GOTO A

* Threads jumps that have the same condition:
        IF E A; else B;
     A: IF E C; else D;
  =>
        IF E C; else B;
  This happens for either branch of an IF.

* Eliminates dependencies on 'not':
        E = (not X)
        if E A; else B;
  =>
        if X B; else A;
  Note that this leaves the computation of E in the code.  This may
  be eliminated later by DCE.

* Similarly, removes (eq X nil) or (eq nil X)

* Converts IF with a constant to a GOTO:
        if <<nil>> A; else B;
  =>
        goto B;

* Converts a `throw' to a `goto' when it is provably correct.
  This can be done when the `catch' and the `throw' both have a
  constant tag argument, and when there are no intervening
  `unwind-protect' calls (this latter restriction could be lifted
  with some more work).

Note that nothing here explicitly removes blocks.  This is not
needed because the only links to blocks are the various branches;
when a block is not needed it will be reclaimed by the garbage
collector."
  (let ((rewrote-one t))
    (while rewrote-one
      (setf rewrote-one nil)
      (elcomp--iterate-over-bbs
       compiler
       (lambda (block)
	 (let ((insn (elcomp--last-instruction block)))
	   ;; See if we can turn a `throw' into a `goto'.  This only
	   ;; works when not in SSA form, because it reuses variable
	   ;; names from the `catch' handler.
	   (unless in-ssa-form
	     (when (elcomp--maybe-replace-catch block insn)
	       (setf rewrote-one t)))
	   ;; A GOTO to a block holding just another branch (of any kind)
	   ;; can be replaced by the instruction at the target.
	   (when (and (elcomp--goto-p insn)
		      ;; Exclude a self-goto.
		      (not (eq block
			       (oref insn :block)))
		      (elcomp--nonreturn-terminator-p
		       (elcomp--first-instruction (oref insn :block))))
	     ;; Note we also set INSN for subsequent optimizations
	     ;; here.
	     (setf insn (elcomp--first-instruction (oref insn :block)))
	     (setf (elcomp--last-instruction block) insn)
	     (setf rewrote-one t))

	   ;; If a target of an IF is a GOTO, the destination can be
	   ;; hoisted.
	   (when (and (elcomp--if-p insn)
		      (elcomp--goto-p (elcomp--first-instruction
					     (oref insn :block-true))))
	     (oset insn :block-true
		   (oref (elcomp--first-instruction (oref insn :block-true))
			 :block))
	     (setf rewrote-one t))
	   (when (and (elcomp--if-p insn)
		      (elcomp--goto-p (elcomp--first-instruction
					     (oref insn :block-false))))
	     (oset insn :block-false
		   (oref (elcomp--first-instruction (oref insn :block-false))
			 :block))
	     (setf rewrote-one t))

	   ;; If both branches of an IF point to the same spot, turn
	   ;; it into a GOTO.
	   (when (and (elcomp--if-p insn)
		      (eq (oref insn :block-true)
			  (oref insn :block-false)))
	     (setf insn (elcomp--goto :block (oref insn :block-true)))
	     (setf (elcomp--last-instruction block) insn)
	     (setf rewrote-one t))

	   ;; If the condition for an IF was a call to 'not', then the
	   ;; call can be bypassed and the targets swapped.
	   (when (and in-ssa-form (elcomp--if-p insn))
	     (elcomp--peel-condition insn))

	   ;; If the argument to the IF is a constant, turn the IF
	   ;; into a GOTO.
	   (when (and in-ssa-form (elcomp--if-p insn))
	     (let ((condition (oref insn :sym)))
	       ;; FIXME could also check for calls known not to return
	       ;; nil.
	       (when (elcomp--constant-p condition)
		 (let ((goto-block (if (oref condition :value)
				       (oref insn :block-true)
				     (oref insn :block-false))))
		   (setf insn (elcomp--goto :block goto-block))
		   (setf (elcomp--last-instruction block) insn)
		   (setf rewrote-one t)))))

	   ;; If a target of an IF is another IF, and the conditions are the
	   ;; same, then the target IF can be hoisted.
	   (when (elcomp--if-p insn)
	     ;; Thread the true branch.
	     (when (and (elcomp--if-p (elcomp--first-instruction
					     (oref insn :block-true)))
			(eq (oref insn :sym)
			    (oref (elcomp--first-instruction
				   (oref insn :block-true))
				  :sym)))
	       (oset insn :block-true
		     (oref (elcomp--first-instruction
			    (oref insn :block-true))
			   :block-true)))
	     ;; Thread the false branch.
	     (when (and (elcomp--if-p (elcomp--first-instruction
					     (oref insn :block-false)))
			(eq (oref insn :sym)
			    (oref (elcomp--first-instruction
				   (oref insn :block-false))
				  :sym)))
	       (oset insn :block-false
		     (oref (elcomp--first-instruction
			    (oref insn :block-false))
			   :block-false)))))))

      (when rewrote-one
	(elcomp--invalidate-cfg compiler)))))

(provide 'elcomp/jump-thread)

;;; jump-thread.el ends here
