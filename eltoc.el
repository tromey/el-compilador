;;; FIXME must be updated to use elcomp

;; We should also allow a declaration that allows a direct C
;; call, not allowing symbol redefinition.
;; (declare (direct FUNC))

(defconst elcomp--c-types
  '((integer "EMACS_INT" "XINT")
    (float "double" "XFLOAT_DATA")
    (symbol "struct Lisp_Symbol *" "XSYMBOL")))

(defconst elcomp--simple-math
  '(< <= > >= /= + - * / 1+))

(defun elcomp--c-get-type (var)
  (let* ((var-type (elcomp--get-type))
	 (type-elt (assoc var-type elcomp--c-types)))
    (if type-elt
	(cadr type-elt)
      "Lisp_Object")))

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

(defun elcomp--c-test (type value-str)
  (if (memq type '(integer float))
      value-str
    (concat "NILP (" value-str ")")))

(defun elcomp--c-generate-inline-code (loc func args)
  (let ((loc-type (get loc :type)))
    (when (eq func '1+)			;FIXME something more generic
      (push 1 args)
      (setq func '+))
    (insert (mapconcat
	     (lambda (item)
	       (elcomp--c-atom-to-expr item loc-type))
	     args (concat " " (symbol-name func) " ")))
    (insert ";\n")))

(defgeneric elcomp--c-generate (insn)
  "Insert C code for INSN into the current buffer.")

(defmethod elcomp--c-generate (insn)
  (error "unrecognized instruction"))

(defmethod elcomp--c-generate ((insn elcomp--set))
  (insert "  " (symbol-name (oref insn :sym)) " = "
	  ...
  )

(defun elcomp--c-generate-code (linear)
  (dolist (item linear)
    (pcase item
      (`(set ,loc ,result)
       (insert "  " (symbol-name loc) " = "
	       (elcomp--c-atom-to-c-expr result
				      (and loc
					   (get loc :type)))
	       ";\n"))
      (`(call ,loc ,func . ,args)
       (insert "  ")
       (if loc
	   (insert (symbol-name loc) " = "))
       (if (and loc
		(get loc :type)
		(memq func elcomp--c-simple-math))
	   (elcomp--c-generate-inline-code loc func args)
	 (insert "F" (elcomp--c-c-name func) " (")
	 (let ((seen nil))
	   (while args
	     (if seen
		 (insert ", ")
	       (setq seen t))
	     (insert (elcomp--c-atom-to-c-expr (car args) nil))
	     (setq args (cdr args))))
	 (insert ");\n")))
      (`(label ,num)
       (insert "l" (int-to-string num) ":\n"))
      (`(goto ,num)
       (insert "  goto l" (int-to-string num) ";\n"))
      (`(if ,sym ,true-label ,false-label . nil)
       (if true-label
	   (progn
	     (insert "  if (!" (elcomp--c-test (get sym :type)
					    (symbol-name sym))
		     ") goto l"
		     (int-to-string true-label)
		     ";\n")
	     (if false-label (insert "  else goto l"
				     (int-to-string false-label) ";\n")))
	 (unless false-label
	   (error "neither true nor false label"))
	 (insert "  if (" (elcomp--c-test (get sym :type)
				       (symbol-name sym))
		 ")) goto l"
		 (int-to-string false-label)
		 ";\n"))))))

(defun elcomp--c-generate-decls ()
  (dolist (var elcomp--c-variables)
    (insert "  " (elcomp--c-get-type var) " " (symbol-name var) ";\n")))

(defun elcomp--c-generate-defun ()
  (let ((c-name (elcomp--c-c-name (car elcomp--c-defun))))	; FIXME mangling
    (insert "DEFUN (\"" (symbol-name (car elcomp--c-defun)) ;FIXME quoting
	    "\", F" c-name
	    ", S" c-name ", FIXME, FIXME,\n" ;args
	    ;; Interactive.
	    ;; FIXME: quoting for the interactive spec
	    ;; Note that we can have a whole lisp form here.
	    "      " (or (nth 3 elcomp--c-defun) "0") ",\n"
	    "       doc: /* "
	    (or (nth 2 elcomp--c-defun) "nothing??") ;FIXME anything?
	    " */)\n"
	    "  (Lisp_Object arg)"	;FIXME args
	    "\n")))

(defun elcomp--c-translate (form buffer)
  (byte-compile-close-variables
   (let* ((byte-compile-macro-environment
	   (cons '(declare . elcomp--c-declare)
		 byte-compile-macro-environment))
	  (elcomp--c-variables nil)
	  (elcomp--c-rewrite-alist nil)
	  (elcomp--c-next-label 0)
	  (elcomp--c-code nil)
	  (elcomp--c-code-link nil)
	  (elcomp--c-defun nil)
	  (elcomp--c-defuns nil)
	  (result-var (elcomp--c-new-var))
	  (code nil))
     (setq form (elcomp--c-extract-defun form))
     (elcomp--c-linearize
      (byte-optimize-form (macroexpand form
				       byte-compile-macro-environment))
      result-var)
     ;; FIXME bad factoring here.
     (with-current-buffer buffer
       (erase-buffer)
       (elcomp--c-generate-defun)
       (insert "{\n")
       (elcomp--c-generate-decls)
       (insert "\n")
       (elcomp--c-generate-code elcomp--c-code)
       (pp elcomp--c-code (current-buffer))
       (insert "  return " (symbol-name result-var) ";\n")
       (insert "}\n")

       ;; This next part belongs in an outer loop.
       (insert "\n\n")
       (insert "void\nsyms_of_FIXME (void)\n{\n")
       (dolist (one-def elcomp--c-defuns)
	 (insert "  defsubr (&S" (elcomp--c-c-name one-def) ");\n"))
       (insert "}\n")))))

;; Temporary function for hacking.
(defun elcomp--c-do (form)
  (let ((buf (get-buffer-create "*ELTOC*")))
    (elcomp--c-translate form buf)
    (pop-to-buffer buf)))
