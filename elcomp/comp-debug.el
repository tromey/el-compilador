;;; comp-debug.el --- Debugging the compiler. -*- lexical-binding:t -*-

;;; Commentary:

;; Debugging helpers for the compiler.

;;; Code:

(require 'elcomp)
(require 'elcomp/linearize)
(require 'elcomp/typeinf)

(defgeneric elcomp--pp (obj verbose)
  "Pretty-print a compiler object.

OBJ is the object to pretty-print.
VERBOSE non-nil means to write a more verbose description.")

(defmethod elcomp--pp (obj verbose)
  (error "unrecognized instruction"))

(defmethod elcomp--pp (obj verbose)
  (princ obj))

;; FIXME eldoc for defmethod is messed up
(defmethod elcomp--pp ((obj elcomp--set) verbose)
  (if verbose
      (progn
	(princ "set ")
	(elcomp--pp (oref obj :sym) nil)
	(princ " = ")
	(elcomp--pp (oref obj :value) nil))
    (elcomp--pp (oref obj :sym) nil)))

(defmethod elcomp--pp ((obj elcomp--call) verbose)
  (if verbose
      (progn
	(princ "call ")
	(elcomp--pp (oref obj :sym) nil)
	(princ " = ")
	(elcomp--pp (oref obj :func) nil)
	(when (oref obj :args)
	  (let ((first t))
	    (dolist (arg (oref obj :args))
	      (princ (if first "(" " "))
	      (setf first nil)
	      (elcomp--pp arg nil))
	    (princ ")"))))
    (elcomp--pp (oref obj :sym) nil)))

(defmethod elcomp--pp ((obj elcomp--goto) verbose)
  (princ "goto BB ")
  (princ (elcomp--basic-block-number (oref obj :block))))

(defmethod elcomp--pp ((obj elcomp--if) verbose)
  (princ "if ")
  (elcomp--pp (oref obj :sym) nil)
  (princ " BB ")
  (princ (elcomp--basic-block-number (oref obj :block-true)))
  (princ " else BB ")
  (princ (elcomp--basic-block-number (oref obj :block-false))))

(defmethod elcomp--pp ((obj elcomp--return) verbose)
  (princ "return ")
  (elcomp--pp (oref obj :sym) nil))

(defmethod elcomp--pp ((obj elcomp--constant) verbose)
  (princ "<< ")
  (princ (oref obj :value))
  (princ " >>"))

(defmethod elcomp--pp ((obj elcomp--phi) verbose)
  (princ "ϕ:")
  (princ (oref obj :original-name))
  (when verbose
    (princ " =")
    (maphash (lambda (item _ignore)
	       (princ " ")
	       (elcomp--pp item nil))
	     (oref obj :args))))

(defmethod elcomp--pp ((obj elcomp--argument) verbose)
  (princ "argument ")
  (princ (oref obj :original-name)))

(defmethod elcomp--pp ((obj elcomp--catch) verbose)
  (princ "catch ")
  (princ (oref obj :tag))
  (princ " => BB ")
  (princ (elcomp--basic-block-number (oref obj :handler))))

(defmethod elcomp--pp ((obj elcomp--condition-case) verbose)
  (princ "condition-case ")
  (princ (oref obj :condition-name))
  (princ " => BB ")
  (princ (elcomp--basic-block-number (oref obj :handler))))

(defmethod elcomp--pp ((obj elcomp--unwind-protect) verbose)
  (princ "unwind-protect => BB ")
  (princ (elcomp--basic-block-number (oref obj :handler))))

(defmethod elcomp--pp ((obj elcomp--fake-unwind-protect) verbose)
  (princ "fake-unwind-protect ")
  (princ (oref obj :count)))

(defun elcomp--pp-insn (text insn verbose)
  (princ text)
  (princ " ")
  (elcomp--pp insn verbose)
  (princ "\n"))

(defun elcomp--pp-basic-block (bb)
  (princ (format "\n[BB %d"
		 (elcomp--basic-block-number bb)))
  (when (and (elcomp--basic-block-parents bb)
	     (> (hash-table-count (elcomp--basic-block-parents bb)) 0))
    (princ " (parents:")
    (maphash (lambda (parent-bb _ignore)
	       (princ (format " %d" (elcomp--basic-block-number parent-bb))))
	     (elcomp--basic-block-parents bb))
    (princ ")"))
  (princ (format " (idom: %s)"
		 (if (elcomp--basic-block-immediate-dominator bb)
		     (elcomp--basic-block-number
		      (elcomp--basic-block-immediate-dominator bb))
		   "nil")))
  (princ "]\n")
  (dolist (exception (elcomp--basic-block-exceptions bb))
    (princ "    ")
    (elcomp--pp exception (current-buffer))
    (princ "\n"))
  (when (elcomp--basic-block-phis bb)
    (maphash (lambda (_ignore_name phi)
	       (princ "    ")
	       (elcomp--pp phi t)
	       (let ((type (elcomp--look-up-type bb phi)))
		 (when type
		   (princ " ; TYPE = ")
		   (princ type)))
	       (princ "\n"))
	     (elcomp--basic-block-phis bb)))
  (dolist (item (elcomp--basic-block-code bb))
    (elcomp--pp item (current-buffer))
    (let ((type (elcomp--look-up-type bb item)))
      (when type
	(princ " ; TYPE = ")
	(princ type)))
    (princ "\n")))

(defun elcomp--pp-compiler (compiler)
  "Pretty-print the contents of COMPILER into the current buffer."
  (elcomp--iterate-over-bbs compiler #'elcomp--pp-basic-block)
  (insert "\n=============================================================\n"))

;; Temporary function for hacking.
(defun elcomp--do (form)
  "Convert the defun FORM to compiler-internal form and print to a buffer.

This is useful for debugging."
  (let ((buf (get-buffer-create "*ELCOMP*")))
    (with-current-buffer buf
      (erase-buffer)
      ;; Use "let*" so we can hack debugging prints into the compiler
      ;; and have them show up in the temporary buffer.
      (let* ((standard-output buf)
	     (compiled-form (elcomp--translate form)))
	(elcomp--pp-compiler compiled-form))
      (pop-to-buffer buf))))

;;; comp-debug.el ends here
