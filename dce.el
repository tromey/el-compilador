;;; dce.el --- Dead Code Elimination.

;;; Commentary:

;; A simple dead code elimination pass.

;;; Code:

(cl-defstruct elcomp--dce
  "A structure that holds the data for a DCE pass.

An object of this type is allocated when `elcomp--dce-pass' is working.
It holds data internal to the pass."
  ;; WORK-LIST holds a list of instructions to mark as needed.
  work-list
  ;; HASH is a hash table whose keys are instructions which have been
  ;; marked as needed.
  (hash (make-hash-table)))

(defgeneric elcomp--mark-necessary (insn dce just-intrinsic)
  "Possibly mark the instruction INSN as necessary.
DCE is the DCE state object for the pass.

If JUST-INTRINSIC is non-nil, then only mark the instruction if
it is intrinsically needed.  If it is nil, then mark the
instruction.

Marking the instruction means adding it to the hash and then
pushing the instruction's arguments onto the work list.")

(defmethod elcomp--mark-necessary (insn dce just-intrinsic)
  "The default case is to mark a statement as needed."
  (puthash insn t (elcomp--dce-hash dce)))

(defmethod elcomp--mark-necessary ((insn elcomp--return) dce just-intrinsic)
  "`If' statements are marked as needed and their argument is pushed."
  ;; An IF is always needed.
  (puthash insn t (elcomp--dce-hash dce))
  ;; And so is its reference.
  (push (oref insn :sym) (elcomp--dce-work-list dce)))

(defmethod elcomp--mark-necessary ((insn elcomp--return) dce just-intrinsic)
  "`Return' statements are marked as needed and their argument is pushed."
  ;; A RETURN is always needed.
  (puthash insn t (elcomp--dce-hash dce))
  ;; And so is its reference.
  (push (oref insn :sym) (elcomp--dce-work-list dce)))

(defmethod elcomp--mark-necessary ((insn elcomp--set) dce just-intrinsic)
  "Mark a `set' statement as necessary.

In the first pass, do nothing.  A `set' is not intrinsically needed.
In the second pass, mark this statement as needed, and then push
its references on the work list."
  ;; A SET is not intrinsically needed, so check which pass this is.
  (unless just-intrinsic
    (puthash insn t (elcomp--dce-hash dce))
    (when (elcomp--ssa-name-p (oref insn :value))
      (push (oref insn :value) (elcomp--dce-work-list dce)))))

(defmethod elcomp--mark-necessary ((insn elcomp--phi) dce just-intrinsic)
  "Mark a `phi' statement as necessary.

In the first pass, do nothing.  A `phi' is not intrinsically needed.
In the second pass, mark this statement as needed, and then push
its references on the work list."
  ;; A PHI is not intrinsically needed, so check which pass this is.
  (unless just-intrinsic
    (puthash insn t (elcomp--dce-hash dce))
    (maphash (lambda (arg _ignore)
	       (push arg (elcomp--dce-work-list dce)))
	     (oref insn :args))))

(defmethod elcomp--mark-necessary ((insn elcomp--call) dce just-intrinsic)
  "Mark a `call' statement as necessary.

In the first pass, do nothing.  A `call' is not intrinsically needed.
In the second pass, mark this statement as needed, and then push
its references on the work list."
  (let ((push-args nil))
    ;; A non-const call is intrinsically needed.  However, we mark it
    ;; specially so we can determine whether its LHS is needed as
    ;; well.  Note that the "const" check also picks up the
    ;; "diediedie" statements.
    (if just-intrinsic
	(unless (elcomp--func-const-p (oref insn :func))
	  (puthash insn :call (elcomp--dce-hash dce))
	  (setf push-args t))
      ;; Otherwise, we're propagating.
      (puthash insn t (elcomp--dce-hash dce))
      (setf push-args t))
    (when push-args
      ;; Push the arguments on the work list.
      (dolist (arg (oref insn :args))
	(when (elcomp--ssa-name-p arg)
	  (push arg (elcomp--dce-work-list work-list)))))))

(defun elcomp--dce-mark-intrinsically-necessary (compiler dce)
  "Mark all intrinsically necessary statements.

This is the first pass of DCE.

Any intrinsically necessary statement is entered into the `hash'
field of DCE and its references are pushed onto `work-list'."
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (dolist (insn (elcomp--basic-block-code bb))
       (elcomp--mark-necessary insn dce t)))))

(defun elcomp--dce-propagate-necessary (compiler dce)
  "Propagate the \"necessary\" property through the function.

This is the second pass of DCE.

This iterates over the work list, entering statements into the
DCE's `hash' table and pushing references onto the `work-list'."
  (while (elcomp--dce-work-list dce)
    (let* ((insn (pop (elcomp--dce-work-list dce)))
	   (mark (gethash insn (elcomp--dce-hash dce))))
      ;; If it is marked as 't', then we don't need to do any more.
      ;; If it is marked as :call, upgrade to 't'.
      (if mark
	  (when (eq mark :call)
	    ;; Upgrade a call.
	    (puthash insn t (elcomp--dce-hash dce)))
	(elcomp--mark-necessary insn dce nil)))))

(defun elcomp--dce-delete-dead-statements (compiler dce)
  "Delete dead statements.

Iterate over the statements in the function and remove any
statement that has not been marked as necessary."
  (let ((hash (elcomp--dce-hash dce)))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       ;; Delete dead statements.
       (let ((iter (elcomp--basic-block-code bb)))
	 (while iter
	   (unless (gethash (car iter) hash)
	     (setcar iter nil))
	   (setf iter (cdr iter))))
       (setf (elcomp--basic-block-code bb)
	     (delq nil (elcomp--basic-block-code bb)))
       ;; Delete dead phi nodes.
       (let ((phi-table (elcomp--basic-block-phis bb)))
	 (maphash (lambda (key _ignore)
		    (unless (gethash key hash)
		      (remhash key phi-table)))
		  phi-table))))))

(defun elcomp--dce-pass (compiler)
  "Delete dead code."
  (let ((dce (make-elcomp--dce)))
    (elcomp--dce-mark-intrinsically-necessary compiler dce)
    (elcomp--dce-propagate-necessary compiler dce)
    (elcomp--dce-delete-dead-statements compiler dce)))

;;; dce.el ends here
