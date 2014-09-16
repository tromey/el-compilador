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

(defun elcomp--compute-back-edges (compiler)
  (elcomp--reset-back-edges compiler)
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (elcomp--add-links (elcomp--last-instruction bb) bb))))
