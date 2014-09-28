;;; back.el - fix up back edges.

;;; Commentary:

;; Reconstruct the back edges in the CFG.

;;; Code:

(defun elcomp--reset-back-edges (compiler)
  "Reset the back edges of all basic blocks in COMPILER.

This sets all the back edges to nil."
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (setf (elcomp--basic-block-parents bb)
	   (make-hash-table)))))

(defgeneric elcomp--add-links (insn block)
  "Add backlinks for the instruction INSN, which appears in BLOCK.")

(defmethod elcomp--add-links (insn block)
  "The base case does nothing.  Most instructions don't have links."
  ;; Do nothing.
  )

(defmethod elcomp--add-links ((insn elcomp--goto) block)
  "Add backlinks for a `goto'."
  (puthash block t (elcomp--basic-block-parents (oref insn :block))))

(defmethod elcomp--add-links ((insn elcomp--if) block)
  "Add backlinks for an `if'."
  (puthash block t (elcomp--basic-block-parents (oref insn :block-true)))
  (puthash block t (elcomp--basic-block-parents (oref insn :block-false))))

(defun elcomp--require-back-edges (compiler)
  "Require the back links in COMPILER to be valid.

If the links are already believed to be valid, this does nothing.
Otherwise, it recreates the links."
  (unless (elcomp--back-edges-valid compiler)
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (dolist (exception (elcomp--basic-block-exceptions bb))
	 (puthash bb t
		  (elcomp--basic-block-parents (oref exception :handler))))
       (elcomp--add-links (elcomp--last-instruction bb) bb)))
    (setf (elcomp--back-edges-valid compiler) t)))

(defun elcomp--invalidate-back-edges (compiler)
  "Invalidate the back links in COMPILER."
  (when (elcomp--back-edges-valid compiler)
    (elcomp--reset-back-edges compiler)
    (setf (elcomp--back-edges-valid compiler) nil)))

;;; back.el ends here
