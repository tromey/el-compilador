;;; eh-cleanup.el --- Clean up exceptions. -*- lexical-binding:t -*-

;;; Commentary:

;; A pass to remove obviously-dead exception edges.

;;; Code:

(defgeneric elcomp--can-throw (insn)
  "Return t if INSN can `throw' or `signal', otherwise nil."
  )

(defmethod elcomp--can-throw (insn)
  "The base case is to assume any instruction can throw."
  t)

(defmethod elcomp--can-throw ((insn elcomp--set))
  "A `set' instruction cannot throw."
  nil)

(defmethod elcomp--can-throw ((insn elcomp--goto))
  "A `goto' instruction cannot throw."
  nil)

(defmethod elcomp--can-throw ((insn elcomp--if))
  "An `if' instruction cannot throw."
  nil)

(defmethod elcomp--can-throw ((insn elcomp--return))
  "A `return' instruction cannot throw."
  nil)

(defmethod elcomp--can-throw ((insn elcomp--call))
  "A `call' instruction usually can throw.
A function marked `nothrow' will not throw."
  ;; Note that we can't really be picky about `signal' or `throw'
  ;; tags, due to QUIT and `throw-on-input'.
  (if (and (symbolp (oref insn :func))
	   (elcomp--func-nothrow-p (oref insn :func)))
      nil
    t))

(defmethod elcomp--can-throw ((insn elcomp--diediedie))
  "A `diediedie' instruction always throws."
  t)

(defun elcomp--eh-remove-unwinds (compiler bb)
  "Remove any empty `unwind-protect' edges from the basic block BB.

An empty `unwind-protect' edge is one where the target block
consists of just a call to the special `:unwind-protect-continue'
function."
  ;; There's probably some cl-loop formulation that isn't so ugly.
  (catch 'done
    (while t
      (let ((exception (car (elcomp--basic-block-exceptions bb))))
	;; Only the outermost exception edge is eligible for removal.
	(unless (elcomp--unwind-protect-p exception)
	  (throw 'done nil))
	(let ((exc-block (oref exception :handler)))
	  ;; If the block is just a single instruction, then we know
	  ;; it is a call to the special :unwind-protect-continue
	  ;; function, and so the edge can be removed.
	  (unless (eq (elcomp--basic-block-code exc-block)
		      (elcomp--basic-block-code-link exc-block))
	    (throw 'done nil))
	  (cl-assert (elcomp--diediedie-p
		      (car (elcomp--basic-block-code exc-block))))
	  (pop (elcomp--basic-block-exceptions bb)))))))

(defun elcomp--eh-cleanup-pass (compiler)
  "Remove useless exception handling edges from a function.

This operates on the function currently being defined in COMPILER.

This pass will remove useless `unwind-protect' edges.  See
`elcomp--eh-remove-unwinds'.

It will also remove all exception edges from a basic block if
that block has no instructions which may throw."
  (let ((found-one nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (elcomp--eh-remove-unwinds compiler bb)
       ;; Don't bother if there are already no exception handlers.
       (when (elcomp--basic-block-exceptions bb)
	 (unless (catch 'can-throw
		   (dolist (insn (elcomp--basic-block-code bb))
		     (when (elcomp--can-throw insn)
		       (throw 'can-throw t))))
	   ;; Since nothing here can throw, we can remove the
	   ;; exception handlers.
	   (setf (elcomp--basic-block-exceptions bb) nil)
	   (setf found-one t)))))
    (when found-one
      (elcomp--invalidate-cfg compiler))))

;;; eh-cleanup.el ends here
