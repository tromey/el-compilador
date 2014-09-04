;;; FIXME must be updated to use elcomp

(defconst eltoc--types
  '((fixnum "EMACS_INT" "XINT")
    (float "double" "XFLOAT_DATA")
    (symbol "struct Lisp_Symbol *" "XSYMBOL")))

(defun eltoc--get-type (var)
  (let* ((var-type (get var :type))
	 (type-elt (assoc var-type eltoc--types)))
    ;; It is an error if the variable has a type which we don't
    ;; recognize.
    (and var-type
	 (not type-elt)
	 (error "type not found"))
    (if type-elt
	(cadr type-elt)
      "Lisp_Object")))

(defun eltoc--c-name (symbol)
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

(defun eltoc--atom-to-c-expr (atom lhs-type)
  (cond
   ((stringp atom)
    atom)				;FIXME: should c-quote and box
   ((integerp atom)			;FIXME: fixnump
    (if (memq lhs-type '(fixnum float))
	(int-to-string atom)
      (format "make_number (%s)" atom)))
   ((symbolp atom)
    (symbol-name atom))
   (t
    (error "???"))))

(defun eltoc--test (type value-str)
  (if (memq type '(fixnum float))
      value-str
    (concat "NILP (" value-str ")")))

(defun eltoc--generate-inline-code (loc func args)
  (let ((loc-type (get loc :type)))
    (if (eq func '1+)			;FIXME something more generic
	(progn
	  (push 1 args)
	  (setq func '+)))
    (insert (mapconcat
	     (lambda (item)
	       (eltoc--atom-to-c-expr item loc-type))
	     args (concat " " (symbol-name func) " ")))
    (insert ";\n")))

(defun eltoc--generate-code (linear)
  (dolist (item linear)
    (pcase item
      (`(set ,loc ,result)
       (insert "  " (symbol-name loc) " = "
	       (eltoc--atom-to-c-expr result
				      (and loc
					   (get loc :type)))
	       ";\n"))
      (`(call ,loc ,func . ,args)
       (insert "  ")
       (if loc
	   (insert (symbol-name loc) " = "))
       (if (and loc
		(get loc :type)
		(memq func eltoc--simple-math))
	   (eltoc--generate-inline-code loc func args)
	 (insert "F" (eltoc--c-name func) " (")
	 (let ((seen nil))
	   (while args
	     (if seen
		 (insert ", ")
	       (setq seen t))
	     (insert (eltoc--atom-to-c-expr (car args) nil))
	     (setq args (cdr args))))
	 (insert ");\n")))
      (`(label ,num)
       (insert "l" (int-to-string num) ":\n"))
      (`(goto ,num)
       (insert "  goto l" (int-to-string num) ";\n"))
      (`(if ,sym ,true-label ,false-label . nil)
       (if true-label
	   (progn
	     (insert "  if (!" (eltoc--test (get sym :type)
					    (symbol-name sym))
		     ") goto l"
		     (int-to-string true-label)
		     ";\n")
	     (if false-label (insert "  else goto l"
				     (int-to-string false-label) ";\n")))
	 (unless false-label
	   (error "neither true nor false label"))
	 (insert "  if (" (eltoc--test (get sym :type)
				       (symbol-name sym))
		 ")) goto l"
		 (int-to-string false-label)
		 ";\n"))))))

(defun eltoc--generate-decls ()
  (dolist (var eltoc--variables)
    (insert "  " (eltoc--get-type var) " " (symbol-name var) ";\n")))

;; We don't need this now that append does it.
;; (defun eltoc--infer-types ()
;;   ;; The stupidest type inference pass ever.
;;   ;; FIXME at least needs to DTRT for calls.
;;   (dolist (stmt eltoc--code)
;;     (pcase stmt
;;       (`(set ,loc ,result)
;;        (let ((type-name (and (symbolp result)
;; 			     (get result :type))))
;; 	 (if type-name
;; 	     (eltoc--set-type loc type-name)))))))

(defun eltoc--generate-defun ()
  (let ((c-name (eltoc--c-name (car eltoc--defun))))	; FIXME mangling
    (insert "DEFUN (\"" (symbol-name (car eltoc--defun)) ;FIXME quoting
	    "\", F" c-name
	    ", S" c-name ", FIXME, FIXME,\n" ;args
	    ;; Interactive.
	    ;; FIXME: quoting for the interactive spec
	    ;; Note that we can have a whole lisp form here.
	    "      " (or (nth 3 eltoc--defun) "0") ",\n"
	    "       doc: /* "
	    (or (nth 2 eltoc--defun) "nothing??") ;FIXME anything?
	    " */)\n"
	    "  (Lisp_Object arg)"	;FIXME args
	    "\n")))

(defun eltoc--translate (form buffer)
  (byte-compile-close-variables
   (let* ((byte-compile-macro-environment
	   (cons '(declare . eltoc--declare)
		 byte-compile-macro-environment))
	  (eltoc--variables nil)
	  (eltoc--rewrite-alist nil)
	  (eltoc--next-label 0)
	  (eltoc--code nil)
	  (eltoc--code-link nil)
	  (eltoc--defun nil)
	  (eltoc--defuns nil)
	  (result-var (eltoc--new-var))
	  (code nil))
     (setq form (eltoc--extract-defun form))
     (eltoc--linearize
      (byte-optimize-form (macroexpand form
				       byte-compile-macro-environment))
      result-var)
     ;; FIXME bad factoring here.
     (with-current-buffer buffer
       (erase-buffer)
       (eltoc--generate-defun)
       (insert "{\n")
       (eltoc--generate-decls)
       (insert "\n")
       (eltoc--generate-code eltoc--code)
       (pp eltoc--code (current-buffer))
       (insert "  return " (symbol-name result-var) ";\n")
       (insert "}\n")

       ;; This next part belongs in an outer loop.
       (insert "\n\n")
       (insert "void\nsyms_of_FIXME (void)\n{\n")
       (dolist (one-def eltoc--defuns)
	 (insert "  defsubr (&S" (eltoc--c-name one-def) ");\n"))
       (insert "}\n")))))

;; Temporary function for hacking.
(defun eltoc--do (form)
  (let ((buf (get-buffer-create "*ELTOC*")))
    (eltoc--translate form buf)
    (pop-to-buffer buf)))
