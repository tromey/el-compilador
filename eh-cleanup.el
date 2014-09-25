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
  ;; FIXME some things are definitely safe.  But note that we can't
  ;; really be picky about `signal' or `throw' tags, due to QUIT and
  ;; `throw-on-input'.
  t)

(defmethod elcomp--eh-nonlocal ((insn elcomp--diediedie))
  t)

(defun elcomp--eh-cleanup-pass (compiler)
  (let ((found-one nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
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
      (elcomp--invalidate-cfg compiler)
      ;; FIXME - good spot to try to coalesce blocks.
      )))
