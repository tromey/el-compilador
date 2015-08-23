;;; back.el --- fix up back edges. -*- lexical-binding:t -*-

;;; Commentary:

;; Reconstruct the back edges in the CFG.

;;; Code:

(require 'elcomp)
(require 'elcomp/iter)

(defun elcomp--reset-back-edges (compiler init)
  "Reset the back edges of all basic blocks in COMPILER.

This sets all the back edges to nil."
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (setf (elcomp--basic-block-parents bb)
	   (if init (make-hash-table) nil)))))

(cl-defgeneric elcomp--add-links (insn block)
  "Add backlinks for the instruction INSN, which appears in BLOCK.")

(cl-defmethod elcomp--add-links (_insn _block)
  "The base case does nothing.  Most instructions don't have links."
  ;; Do nothing.
  )

(cl-defmethod elcomp--add-links ((insn elcomp--goto) block)
  "Add backlinks for a `goto'."
  (puthash block t (elcomp--basic-block-parents (elcomp--block insn))))

(cl-defmethod elcomp--add-links ((insn elcomp--if) block)
  "Add backlinks for an `if'."
  (puthash block t (elcomp--basic-block-parents (elcomp--block-true insn)))
  (puthash block t (elcomp--basic-block-parents (elcomp--block-false insn))))

(defun elcomp--require-back-edges (compiler)
  "Require the back links in COMPILER to be valid.

If the links are already believed to be valid, this does nothing.
Otherwise, it recreates the links."
  (unless (elcomp--back-edges-valid compiler)
    (elcomp--reset-back-edges compiler t)
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (dolist (exception (elcomp--basic-block-exceptions bb))
	 (when (oref exception :handler)
	   (puthash bb t
		    (elcomp--basic-block-parents (oref exception :handler)))))
       (elcomp--add-links (elcomp--last-instruction bb) bb)))
    (setf (elcomp--back-edges-valid compiler) t)))

(defun elcomp--invalidate-back-edges (compiler)
  "Invalidate the back links in COMPILER."
  (when (elcomp--back-edges-valid compiler)
    (elcomp--reset-back-edges compiler nil)
    (setf (elcomp--back-edges-valid compiler) nil)))

(declare-function elcomp--clear-dominators "elcomp/dom")

(defun elcomp--invalidate-cfg (compiler)
  (elcomp--clear-dominators compiler)
  (elcomp--invalidate-back-edges compiler))

(provide 'elcomp/back)

;;; back.el ends here
