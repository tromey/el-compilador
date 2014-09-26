;;; eh-cleanup.el - Clean up exceptions.

(defgeneric elcomp--eh-nonlocal (insn)
  "Return t if INSN can `throw' or `signal'."
  )

(defmethod elcomp--eh-nonlocal (insn)
  "The base case is to assume any instruction can throw."
  t)

(defmethod elcomp--eh-nonlocal ((insn elcomp--set))
  nil)

(defmethod elcomp--eh-nonlocal ((insn elcomp--goto))
  nil)

(defmethod elcomp--eh-nonlocal ((insn elcomp--if))
  nil)

(defmethod elcomp--eh-nonlocal ((insn elcomp--return))
  nil)

(defmethod elcomp--eh-nonlocal ((insn elcomp--call))
  ;; Note that we can't really be picky about `signal' or `throw'
  ;; tags, due to QUIT and `throw-on-input'.
  (if (and (symbolp (oref insn :func))
	   (elcomp--func-nothrow-p (oref insn :func)))
      nil
    t))

(defmethod elcomp--eh-nonlocal ((insn elcomp--diediedie))
  t)

(defun elcomp--eh-remove-unwinds (compiler bb)
  "Remove any empty `unwind-protect' edges from BB."
  ;; There's probably some cl-loop formulation that isn't so ugly.
  (catch 'done
    (while t
      (let ((exception (car (elcomp--basic-block-exceptions bb))))
	;; Only the outermost thing is elig
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
  (let ((found-one nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (elcomp--eh-remove-unwinds compiler bb)
       ;; Don't bother if there are already no exception handlers.
       (when (elcomp--basic-block-exceptions bb)
	 (unless (catch 'can-throw
		   (dolist (insn (elcomp--basic-block-code bb))
		     (when (elcomp--eh-nonlocal insn)
		       (throw 'can-throw t))))
	   ;; Since nothing here can throw, we can remove the
	   ;; exception handlers.
	   (setf (elcomp--basic-block-exceptions bb) nil)
	   (setf found-one t)))))
    (when found-one
      (elcomp--invalidate-cfg compiler))))
