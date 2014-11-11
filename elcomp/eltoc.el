;;; eltoc.el --- compile to C. -*- lexical-binding:t -*-

;;; Commentary:

;; A backend to generate Emacs-flavored C.

;;; Code:

;; We should also allow a declaration that allows a direct C
;; call, not allowing symbol redefinition.
;; (declare (direct FUNC))

(require 'elcomp)
(require 'elcomp/linearize)
(require 'elcomp/props)

(cl-defstruct elcomp--c
  decls
  decl-marker)

(defconst elcomp--c-types
  "Map Lisp types to unchecked C accessor macros."
  '((integer "XINT")
    (float "XFLOAT_DATA")
    (symbol "XSYMBOL")))

(defconst elcomp--simple-math
  '(< <= > >= /= + - * / 1+))

(defun elcomp--c-name (symbol)
  "Compute the C name for a symbol."
  (mapconcat
   (lambda (char)
     (char-to-string
      (cond
       ((eq char ?-) ?_)
       ;; FIXME reject bad stuff
       ;; FIXME check for dups!
       (t char))))
   (symbol-name symbol) ""))

(defun elcomp--c-atom-to-expr (atom lhs-type)
  (cond
   ((stringp atom)
    atom)				;FIXME: should c-quote and box
   ((integerp atom)			;FIXME: integerp
    (if (memq lhs-type '(integer float))
	(int-to-string atom)
      (format "make_number (%s)" atom)))
   ((symbolp atom)
    (symbol-name atom))
   (t
    (error "???"))))

(defun elcomp--c-declare (eltoc sym)
  (unless (gethash sym (elcomp--c-decls eltoc))
    (save-excursion
      (goto-char (elcomp--c-decl-marker eltoc))
      (insert "  Lisp_Object " (elcomp--c-name sym) ";\n")
      (puthash sym t (elcomp--c-decls eltoc)))))

(defun elcomp--c-symbol (eltoc sym)
  (elcomp--c-declare eltoc sym)
  (insert (elcomp--c-name sym)))

(defun elcomp--c-emit-symref (eltoc insn)
  (cond
   ((symbolp insn)
    (elcomp--c-symbol eltoc insn))
   ((elcomp--set-child-p insn)
    (elcomp--c-symbol eltoc (oref insn :name)))
   ((elcomp--call-child-p insn)
    (elcomp--c-symbol eltoc (oref insn :sym)))
   ((elcomp--phi-child-p insn)
    ;; FIXME??
    (elcomp--c-symbol eltoc (oref insn :original-name)))
   (t
    (error "unhandled case: %S" insn))))

(defun elcomp--c-emit-label (block)
  (insert (format "BB_%d" (elcomp--basic-block-number block))))

(defgeneric elcomp--c-emit (insn eltoc)
  "FIXME")

(defmethod elcomp--c-emit (insn _eltoc)
  (error "unhandled case: %S" insn))

(defmethod elcomp--c-emit ((insn elcomp--set) eltoc)
  (elcomp--c-emit-symref eltoc insn)
  (insert " = ")
  (elcomp--c-emit-symref eltoc (oref insn :value)))

(defmethod elcomp--c-emit ((insn elcomp--call) eltoc)
  (elcomp--c-emit-symref eltoc (oref insn :sym))
  (insert " = ")
  (let ((arg-list (oref insn :args))
	(is-direct (elcomp--func-direct-p (oref insn :func))))
    (if is-direct
	(insert "F" (elcomp--c-name (oref insn :func)) " (")
      (push (oref insn :func) arg-list)
      ;; FIXME - what if not a symbol, etc.
      (insert (format "Ffuncall (%d, ((Lisp_Object[]) { " (length arg-list))))
    (let ((first t))
      (dolist (arg arg-list)
	(if first
	    (setf first nil)
	  (insert ", "))
	(elcomp--c-emit-symref eltoc arg)))
    (if is-direct
	(insert ")")
      (insert " }))"))))

(defmethod elcomp--c-emit ((insn elcomp--goto) _eltoc)
  (insert "goto ")
  (elcomp--c-emit-label (oref insn :block)))

(defmethod elcomp--c-emit ((insn elcomp--if) eltoc)
  (insert "if (!NILP (")
  (elcomp--c-emit-symref eltoc (oref insn :sym))
  (insert ")) goto ")
  (elcomp--c-emit-label (oref insn :block-true))
  (insert "; else goto ")
  (elcomp--c-emit-label (oref insn :block-false)))

(defmethod elcomp--c-emit ((insn elcomp--return) eltoc)
  (insert "return ")
  (elcomp--c-emit-symref eltoc (oref insn :sym)))

;; (defmethod elcomp--c-emit ((insn elcomp--constant))
;;   (insert "goto ")
;;   (elcomp--c-emit-label (oref insn :goto)))

(defmethod elcomp--c-emit ((insn elcomp--argument) _eltoc)
  (insert "goto ")
  (elcomp--c-emit-label (oref insn :goto)))

(defun elcomp--c-emit-block (eltoc bb)
  (elcomp--c-emit-label bb)
  (insert ":\n")
  (dolist (insn (elcomp--basic-block-code bb))
    (insert "  ")
    (elcomp--c-emit insn eltoc)
    (insert ";\n")))

(defun elcomp--c-generate-defun (compiler)
  (let* ((info (elcomp--defun compiler))
	 (sym (car info))
	 (c-name (elcomp--c-name sym))) ; FIXME mangling
    (insert "DEFUN (\"" (symbol-name sym) ;FIXME quoting
	    "\", F" c-name
	    ", S" c-name ", FIXME, FIXME,\n" ;args
	    ;; Interactive.
	    ;; FIXME: quoting for the interactive spec
	    ;; Note that we can have a whole lisp form here.
	    "      " (or (nth 3 info) "0") ",\n"
	    "       doc: /* "
	    (or (nth 2 info) "nothing??") ;FIXME anything?
	    " */)\n"
	    "  (Lisp_Object arg)"	;FIXME args
	    "\n")))

(defun elcomp--c-translate-one (compiler)
  (let ((eltoc (make-elcomp--c :decls (make-hash-table)
			       :decl-marker (make-marker))))
    (elcomp--c-generate-defun compiler)
    (insert "{\n")
    (set-marker (elcomp--c-decl-marker eltoc) (point))
    (insert "\n")
    (set-marker-insertion-type (elcomp--c-decl-marker eltoc) t)
    (elcomp--iterate-over-bbs compiler
			      (lambda (bb)
				(elcomp--c-emit-block eltoc bb)))
    (insert "}\n\n")
    (set-marker (elcomp--c-decl-marker eltoc) nil)))

(defun elcomp--c-translate (unit)
  (maphash
   (lambda (_ignore compiler)
     (elcomp--c-translate-one compiler))
   (elcomp--compilation-unit-defuns unit))
  (insert "\n")
  (insert "void\nsyms_of_FIXME (void)\n{\n")
  (maphash
   (lambda (_ignore compiler)
     (let ((name (car (elcomp--defun compiler))))
       (when name
	 (insert "  defsubr (&S" (elcomp--c-name name) ");\n"))))
   (elcomp--compilation-unit-defuns unit))
  (insert "}\n"))

(provide 'elcomp/eltoc)

;;; eltoc.el ends here
