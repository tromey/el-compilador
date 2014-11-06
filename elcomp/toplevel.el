;;; toplevel.el --- compiler top level.  -*- lexical-binding:t -*-

;;; Commentary:

;; Top level interface to compiler.

;;; Code:

(require 'elcomp/coalesce)
(require 'elcomp/cprop)
(require 'elcomp/dom)
(require 'elcomp/eh-cleanup)
(require 'elcomp/jump-thread)
(require 'elcomp/ssa)
(require 'elcomp/typeinf)
(require 'bytecomp)

(defun elcomp--extract-defun (compiler form)
  (unless (eq 'defun (car form))
    (error "not a defun"))
  (push (cadr form) (elcomp--defuns compiler))
  (setf (elcomp--defun compiler)
	(list (cadr form) (cl-caddr form)))
  (setf form (cl-cdddr form))
  ;; The doc string.
  (if (stringp (car form))
      (progn
	(setf (elcomp--defun compiler)
	      (nconc (elcomp--defun compiler) (list (car form))))
	(setf form (cdr form)))
    (setf (elcomp--defun compiler) (nconc (elcomp--defun compiler) nil)))
  ;; Skip declarations.
  (while (and (consp (car form))
	      (eq (caar form) 'declare))
    (setf form (cdr form)))
  ;; Interactive spec.
  (if (and (consp (car form))
	   (eq (caar form) 'interactive))
      (progn
	(setf (elcomp--defun compiler)
	      (nconc (elcomp--defun compiler) (list (cl-cadar form))))
	(setf form (cdr form)))
    (setf (elcomp--defun compiler) (nconc (elcomp--defun compiler) nil)))
  (cons 'progn form))

(defun elcomp--optimize (compiler)
  (elcomp--thread-jumps-pass compiler nil)
  (elcomp--eh-cleanup-pass compiler)
  (elcomp--coalesce-pass compiler)
  (elcomp--compute-dominators compiler)	; don't really need this right now
  (elcomp--into-ssa-pass compiler)
  (elcomp--cprop-pass compiler)
  (elcomp--thread-jumps-pass compiler t)
  (elcomp--coalesce-pass compiler)
  (elcomp--dce-pass compiler)
  (elcomp--infer-types-pass compiler))

;; See bug #18971.
(defvar byte-compile-free-assignments)
(defvar byte-compile-free-references)
(defvar byte-compile--outbuffer)

(defun elcomp--translate (form)
  (byte-compile-close-variables
   (let* ((byte-compile-macro-environment
	   (cons '(condition-case . elcomp--macro-condition-case)
		 (cons '(declare . elcomp--declare)
		       byte-compile-macro-environment)))
	  (compiler (make-elcomp))
	  (result-var (elcomp--new-var compiler)))
     (setf (elcomp--entry-block compiler) (elcomp--label compiler))
     (setf (elcomp--current-block compiler) (elcomp--entry-block compiler))
     (setf form (elcomp--extract-defun compiler form))
     (elcomp--linearize compiler
      (byte-optimize-form (macroexpand-all form
					   byte-compile-macro-environment))
      result-var)
     (elcomp--add-return compiler result-var)
     (elcomp--optimize compiler)
     compiler)))

(provide 'elcomp/toplevel)

;;; toplevel.el ends here
