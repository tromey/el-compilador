;;; phiopt.el --- Phi optimizations. -*- lexical-binding:t -*-

;;; Commentary:

;; Some simple optimizations to remove and simplify unneeded phi
;; nodes.  This is necessary due to the simplistic way that we
;; construct SSA form.

;;; Code:

(require 'elcomp)

(defun elcomp--ssa-hash-sole-key (hash)
  (let ((element))
    (maphash (lambda (key _ignore) (setf element key)) hash)
    element))

(defun elcomp--phiopt-lookup (obj replacers)
  (let ((cell (memq obj replacers)))
    (if cell
	(cdr cell)
      obj)))

(defgeneric elcomp--ssa-substitute (insn replacers))

(defmethod elcomp--ssa-substitute (_insn _replacers)
  nil)

(defmethod elcomp--ssa-substitute ((insn elcomp--set) replacers)
  (setf (oref insn :value)
	(elcomp--phiopt-lookup (oref insn :value) replacers)))

(defmethod elcomp--ssa-substitute ((insn elcomp--call) replacers)
  ;; FIXME the :func slot.
  (let ((cell (oref insn :args)))
    (while cell
      (setf (car cell) (elcomp--phiopt-lookup (car cell) replacers))
      (setf cell (cdr cell)))))

(defmethod elcomp--ssa-substitute ((insn elcomp--if) replacers)
  (setf (oref insn :sym)
	(elcomp--phiopt-lookup (oref insn :sym) replacers)))

(defmethod elcomp--ssa-substitute ((insn elcomp--return) replacers)
  (setf (oref insn :sym)
	(elcomp--phiopt-lookup (oref insn :sym) replacers)))

(defmethod elcomp--ssa-substitute ((insn elcomp--set) replacers)
  (setf (oref insn :value)
	(elcomp--phiopt-lookup (oref insn :value) replacers)))

;; THIS IS BOGUS
;; it doesn't account for phis that are propagated to other BBs.

(defun elcomp--phi-opt-pass (compiler)
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (let ((replacers nil))
       (maphash	(lambda (phi)
		  (when (eq (hash-table-size (oref phi :args)) 1)
		    (push (cons phi
				(elcomp--ssa-hash-sole-key (oref phi :args)))
			  replacers)
		    (remhash phi (elcomp--basic-block-phis bb))))
		(elcomp--basic-block-phis bb))
       (when replacers
	 (dolist (insn (elcomp--basic-block-code bb))
	   ;; FIXME it's lame that this isn't merged with
	   ;; elcomp--ssa-rename.
	   (elcomp--ssa-substitute insn replacers)))))))

(provide 'elcomp/phiopt)

;;; phiopt.el ends here
