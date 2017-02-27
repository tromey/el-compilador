;;; comp-debug.el --- Debugging the compiler. -*- lexical-binding:t -*-

;;; Commentary:

;; Debugging helpers for the compiler.

;;; Code:

(require 'elcomp)
(require 'elcomp/typeinf)

(cl-defgeneric elcomp--pp (obj _verbose)
  "Pretty-print a compiler object.

OBJ is the object to pretty-print.
VERBOSE non-nil means to write a more verbose description."
  (princ obj))

;; FIXME eldoc for cl-defmethod is messed up
(cl-defmethod elcomp--pp ((obj elcomp--set) verbose)
  (if verbose
      (progn
	(princ "set ")
	(elcomp--pp (elcomp--sym obj) nil)
	(princ " = ")
	(elcomp--pp (elcomp--value obj) nil))
    (elcomp--pp (elcomp--sym obj) nil)))

(cl-defmethod elcomp--pp ((obj elcomp--call) verbose)
  (if verbose
      (progn
	(princ "call ")
	(elcomp--pp (elcomp--sym obj) nil)
	(princ " = ")
	(elcomp--pp (elcomp--func obj) nil)
	(when (elcomp--args obj)
	  (let ((first t))
	    (dolist (arg (elcomp--args obj))
	      (princ (if first "(" " "))
	      (setf first nil)
	      (elcomp--pp arg nil))
	    (princ ")"))))
    (elcomp--pp (elcomp--sym obj) nil)))

(cl-defmethod elcomp--pp ((obj elcomp--goto) _verbose)
  (princ "goto BB ")
  (princ (elcomp--basic-block-number (elcomp--block obj))))

(cl-defmethod elcomp--pp ((obj elcomp--if) _verbose)
  (princ "if ")
  (elcomp--pp (elcomp--sym obj) nil)
  (princ " BB ")
  (princ (elcomp--basic-block-number (elcomp--block-true obj)))
  (princ " else BB ")
  (princ (elcomp--basic-block-number (elcomp--block-false obj))))

(cl-defmethod elcomp--pp ((obj elcomp--return) _verbose)
  (princ "return ")
  (elcomp--pp (elcomp--sym obj) nil))

(cl-defmethod elcomp--pp ((obj elcomp--constant) _verbose)
  (princ "<< ")
  (princ (elcomp--value obj))
  (princ " >>"))

(cl-defmethod elcomp--pp ((obj elcomp--phi) verbose)
  (princ "ϕ:")
  (princ (elcomp--original-name obj))
  (when verbose
    (princ " =")
    (maphash (lambda (item _ignore)
	       (princ " ")
	       (elcomp--pp item nil))
	     (elcomp--args obj))))

(cl-defmethod elcomp--pp ((obj elcomp--argument) _verbose)
  (princ "argument ")
  (princ (elcomp--original-name obj)))

(cl-defmethod elcomp--pp ((obj elcomp--catch) _verbose)
  (princ "catch ")
  (princ (elcomp--tag obj))
  (princ " => BB ")
  (princ (elcomp--basic-block-number (elcomp--handler obj))))

(cl-defmethod elcomp--pp ((obj elcomp--condition-case) _verbose)
  (princ "condition-case ")
  (princ (elcomp--condition-name obj))
  (princ " => BB ")
  (princ (elcomp--basic-block-number (elcomp--handler obj))))

(cl-defmethod elcomp--pp ((obj elcomp--unwind-protect) _verbose)
  (princ "unwind-protect => BB ")
  (princ (elcomp--basic-block-number (elcomp--handler obj))))

(cl-defmethod elcomp--pp ((obj elcomp--fake-unwind-protect) _verbose)
  (princ "fake-unwind-protect ")
  (princ (elcomp--count obj)))

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

(defun elcomp--pp-compiler (compiler &optional title)
  "Pretty-print the contents of COMPILER into the current buffer."
  (when title
    (insert "==== " title "\n"))
  (elcomp--iterate-over-bbs compiler #'elcomp--pp-basic-block)
  (insert "\n=============================================================\n"))

(defun elcomp--pp-unit (unit)
  (maphash (lambda (_ignore compiler) (elcomp--pp-compiler compiler))
	   (elcomp--compilation-unit-defuns unit)))

(provide 'elcomp/comp-debug)

;;; comp-debug.el ends here
