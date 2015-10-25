;;; cprop.el --- Constant propagation. -*- lexical-binding:t -*-

;;; Commentary:

;; This pass does constant propagation, copy propagation, and
;; evaluation of pure functions.

;;; Code:

(require 'elcomp)
(require 'elcomp/dce)
(require 'elcomp/props)
(require 'elcomp/subst)

(defun elcomp--cprop-insert (rewrite-map from to)
  (puthash from (or (gethash to rewrite-map) to) rewrite-map))

(defun elcomp--cprop-basic (compiler)
  "Do constant and copy propagation.
Return non-nil if anything was changed."
  (let ((rewrites nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (dolist (insn (elcomp--basic-block-code bb))
	 ;; We can eliminate SET instructions in general.  This
	 ;; handles both constant and copy propagation.
	 (when (elcomp--set-p insn)
	   (unless rewrites
	     (setf rewrites (make-hash-table)))
	   (elcomp--cprop-insert rewrites insn (elcomp--value insn))))))

    (when rewrites
      (elcomp--rewrite-using-map compiler rewrites)
      t)))

(cl-defun elcomp--all-arguments-constant (call)
  (dolist (arg (elcomp--args call))
    (unless (elcomp--constant-p arg)
      (cl-return-from elcomp--all-arguments-constant nil)))
  t)

(defun elcomp--cprop-pure (compiler)
  (let ((rewrites (make-hash-table)))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       ;; Remove phis that have a single argument.
       ;; FIXME with a loop can we see ϕ1 -> ϕ1 ϕ2?
       ;; That is a self-reference?
       (maphash
	(lambda (_ignore phi)
	  (when (eq (hash-table-count (elcomp--args phi)) 1)
	    (elcomp--cprop-insert rewrites phi
				  (elcomp--any-hash-key (elcomp--args phi)))))
	(elcomp--basic-block-phis bb))
       ;; Perform other optimizations.
       (dolist (insn (elcomp--basic-block-code bb))
	 (when (and (elcomp--call-p insn)
		    (elcomp--func-pure-p (elcomp--func insn))
		    (elcomp--all-arguments-constant insn))
	   (let ((new-value
		  (apply (elcomp--func insn)
			 (mapcar (lambda (arg)
				   (elcomp--value arg))
				 (elcomp--args insn)))))
	     (elcomp--cprop-insert rewrites insn
				   (elcomp--constant :value new-value)))))))

    (when (> (hash-table-count rewrites) 0)
      (elcomp--rewrite-using-map compiler rewrites)
      t)))

(defun elcomp--cprop-pass (compiler)
  "A constant- and copy-propagation pass.

This pass operates on COMPILER, performing constant- and
copy-propagation.  It also evaluates `pure' functions and removes
unnecessary phis."
  (while (and (elcomp--cprop-basic compiler)
	      (prog1
		  (elcomp--cprop-pure compiler)
		(elcomp--dce-pass compiler)))
    ;; Nothing.
    nil))

(provide 'elcomp/cprop)

;;; cprop.el ends here
