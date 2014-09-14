;;; Jump-threading pass.

;;; Commentary:

;; This implements a simple jump-threading pass.  See the doc string
;; of elcomp--thread-jumps-pass for details.

;;; Code:

(defun elcomp--get-not-argument (insn)
  "Check if INSN uses a 'not' condition.

INSN is an 'if' instruction.  If the condition was defined by a
call to 'not' (or 'null'), return the argument to the 'not'.
Otherwise return nil."
  (let ((call (oref insn :sym)))
    (if (and (elcomp--call-child-p call)
	     (memq (oref call :func) '(not null)))
	(car (oref call :args)))))

(defun elcomp--thread-jumps-pass (compiler)
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
	   ;; A GOTO to a block holding just another branch (of any kind)
	   ;; can be replaced by the instruction at the target.
	   (when (and (elcomp--goto-child-p insn)
		      (elcomp--nonreturn-terminator-p
		       (elcomp--first-instruction (oref insn :block))))
	     ;; Note we also set INSN for subsequent optimizations
	     ;; here.
	     (setf insn (elcomp--first-instruction (oref insn :block)))
	     (setf (elcomp--last-instruction block) insn)
	     (setf rewrote-one t))

	   ;; If a target of an IF is a GOTO, the destination can be
	   ;; hoisted.
	   (when (and (elcomp--if-child-p insn)
		      (elcomp--goto-child-p (elcomp--first-instruction
					     (oref insn :block-true))))
	     (oset insn :block-true
		   (oref (elcomp--first-instruction (oref insn :block-true))
			 :block))
	     (setf rewrote-one t))
	   (when (and (elcomp--if-child-p insn)
		      (elcomp--goto-child-p (elcomp--first-instruction
					     (oref insn :block-false))))
	     (oset insn :block-false
		   (oref (elcomp--first-instruction (oref insn :block-false))
			 :block))
	     (setf rewrote-one t))

	   ;; If both branches of an IF point to the same spot, turn
	   ;; it into a GOTO.
	   (when (and (elcomp--if-child-p insn)
		      (eq (oref insn :block-true)
			  (oref insn :block-false)))
	     (setf insn (elcomp--goto "goto" :block (oref insn :block-true)))
	     (setf (elcomp--last-instruction block) insn)
	     (setf rewrote-one t))

	   ;; If the condition for an IF was a call to 'not', then the
	   ;; call can be bypassed and the targets swapped.
	   (if nil
	       ;; FIXME - this is not ready yet, as it relies on
	       ;; really having SSA form.
	       (when (elcomp--if-child-p insn)
		 ;; Peel any number of 'not's.
		 (let ((arg-to-not nil))
		   (while (setf arg-to-not (elcomp--get-not-argument insn))
		     (cl-rotatef (oref insn :block-true)
				 (oref insn :block-false))
		     (setf (oref insn :sym) arg-to-not))))
	     )

	   ;; If a target of an IF is another IF, and the conditions are the
	   ;; same, then the target IF can be hoisted.
	   (when (elcomp--if-child-p insn)
	     ;; Thread the true branch.
	     (when (and (elcomp--if-child-p (elcomp--first-instruction
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
	     (when (and (elcomp--if-child-p (elcomp--first-instruction
					     (oref insn :block-false)))
			(eq (oref insn :sym)
			    (oref (elcomp--first-instruction
				   (oref insn :block-false))
				  :sym)))
	       (oset insn :block-false
		     (oref (elcomp--first-instruction
			    (oref insn :block-false))
			   :block-false))))))))))
