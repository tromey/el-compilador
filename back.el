;; Fix up back edges.

(defun elcomp--reset-back-edges (compiler)
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (setf (elcomp--basic-block-parents bb)
	   (make-hash-table)))))

(defgeneric elcomp--add-links (insn block)
  "Add backlinks")

(defmethod elcomp--add-links (insn block)
  ;; Do nothing.
  )

(defmethod elcomp--add-links ((insn elcomp--goto) block)
  (puthash block t (elcomp--basic-block-parents (oref insn :block))))

(defmethod elcomp--add-links ((insn elcomp--if) block)
  (puthash block t (elcomp--basic-block-parents (oref insn :block-true)))
  (puthash block t (elcomp--basic-block-parents (oref insn :block-false))))

(defun elcomp--require-back-edges (compiler)
  (unless (elcomp--back-edges-valid compiler)
    (elcomp--reset-back-edges compiler)
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (dolist (exception (elcomp--basic-block-exceptions bb))
	 (puthash bb t
		  (elcomp--basic-block-parents (oref exception :handler))))
       (elcomp--add-links (elcomp--last-instruction bb) bb)))
    (setf (elcomp--back-edges-valid compiler) t)))

(defun elcomp--invalidate-back-edges (compiler)
  (setf (elcomp--back-edges-valid compiler) nil))
