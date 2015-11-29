;;; name-map.el - Map names to  blocks. -*- lexical-binding:t -*-

;;; Commentary:

;; This has some utility functions to construct a map that maps SSA
;; names to their defining blocks.  This is only needed due to IR
;; deficiencies and should probably be fixed a different way.

;;; Code:

(require 'elcomp)

(cl-defgeneric elcomp--update-name-map (_insn _bb _map)
  ;; Ignore most instructions.
  nil)

(cl-defmethod elcomp--update-name-map ((insn elcomp--set) bb map)
  (puthash insn bb map))

(cl-defmethod elcomp--update-name-map ((insn elcomp--call) bb map)
  (when (elcomp--sym insn)
    (puthash insn bb map)))

(defun elcomp--make-name-map (compiler)
  (let ((name-map (make-hash-table)))
    (dolist (arg (elcomp--arguments compiler))
      (puthash arg (elcomp--entry-block compiler) name-map))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (maphash (lambda (_name phi) (puthash phi bb name-map))
		(elcomp--basic-block-phis bb))
       (dolist (insn (elcomp--basic-block-code bb))
	 (elcomp--update-name-map insn bb name-map))))
    name-map))

(provide 'elcomp/name-map)

;;; name-map.el ends here
