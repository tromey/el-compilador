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
	 (when (elcomp--set-child-p insn)
	   (unless rewrites
	     (setf rewrites (make-hash-table)))
	   (elcomp--cprop-insert rewrites insn (oref insn :value))))))

    (when rewrites
      (elcomp--rewrite-using-map compiler rewrites)
      t)))

(defun elcomp--all-arguments-constant (call)
  (catch 'done
    (dolist (arg (oref call :args))
      (unless (elcomp--constant-child-p arg)
	(throw 'done nil)))
    t))

(defun elcomp--cprop-pure (compiler)
  (let ((rewrites nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (dolist (insn (elcomp--basic-block-code bb))
	 (when (and (elcomp--call-child-p insn)
		    (elcomp--func-pure-p (oref insn :func))
		    (elcomp--all-arguments-constant insn))
	   (let ((new-value
		  (apply (oref insn :func)
			 (mapcar (lambda (arg)
				   (oref arg :value))
				 (oref insn :args)))))
	     (unless rewrites
	       (setf rewrites (make-hash-table)))
	     (elcomp--cprop-insert rewrites insn
				   (elcomp--constant "constant"
						     :value new-value)))))))

    (when rewrites
      (elcomp--rewrite-using-map compiler rewrites)
      t)))

(defun elcomp--cprop-pass (compiler)
  "A constant- and copy-propagation pass.

This pass operates on COMPILER, performing constant- and
copy-propagation.  It also evaluates `pure' functions."
  (let ((keep-going t))
    (while keep-going
      (setf keep-going nil)
      (when (elcomp--cprop-basic compiler)
	(setf keep-going (elcomp--cprop-pure compiler))
	(elcomp--dce-pass compiler)))))

;;; cprop.el ends here
