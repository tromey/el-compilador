;;; cmacros.el --- Compiler macros. -*- lexical-binding:t -*-

;;; Commentary:

;; Some compiler macros used by this compiler.

;;; Code:

(require 'elcomp)

(defun elcomp--macro-declare (&rest specs)
  "A compiler macro for `declare'.

This just ensures we preserve the declaration so the compiler can
see it."
  (cons 'declare specs))

(defun elcomp--macro-condition-case (var bodyform &rest handlers)
  "A compiler macro for `condition-case'.

This pushes VAR as a let-binding into HANDLERS, when VAR is not
nil."
  ;; Use a special name so we (us humans hacking on this) don't get
  ;; confused later on.
  (append (list :elcomp-condition-case bodyform)
	  (if var
	      (mapcar (lambda (handler)
			(list (car handler)
			      `(let ((,var (:elcomp-fetch-condition)))
				 ,@(cdr handler))))
		      handlers)
	    handlers)))

(defun elcomp--macro-save-current-buffer (&rest body)
  (let ((sym (cl-gensym)))
    `(let ((,sym (current-buffer)))
       (unwind-protect
	   (progn ,@body)
	 (if (buffer-live-p sym)
	     (set-buffer sym))))))

(defun elcomp--macro-save-excursion (&rest body)
  (let ((sym (cl-gensym)))
    `(let ((,sym (:save-excursion-save)))
       (unwind-protect
	   (progn ,@body)
	 (:save-excursion-restore sym)))))

(defun elcomp--macro-save-restriction (&rest body)
  (let ((sym (cl-gensym)))
    `(let ((,sym (:save-restriction-save)))
       (unwind-protect
	   (progn ,@body)
	 (:save-restriction-restore sym)))))

(defvar elcomp--compiler-macros
  '((declare . elcomp--macro-declare)
    (condition-case . elcomp--macro-condition-case)
    (save-current-buffer . elcomp--macro-save-current-buffer)
    (save-excursion . elcomp--macro-save-excursion)
    (save-restriction . elcomp--macro-save-restriction)))

(provide 'elcomp/cmacros)

;;; cmacros.el ends here
