;;; eltoc.el --- compile to C. -*- lexical-binding:t -*-

;;; Commentary:

;; A backend to generate Emacs-flavored C.

;; TO DO:
;; emit constants properly
;; handle phi nodes
;; emit lambdas without using DEFUN; call them directly
;;
;; We should also allow a declaration that allows a direct C
;; call, not allowing symbol redefinition.
;; (declare (direct FUNC))

;;; Code:

(require 'subr-x)
(require 'elcomp)
(require 'elcomp/c-inl)
(require 'elcomp/c-renames)
(require 'elcomp/dom)
(require 'elcomp/linearize)
(require 'elcomp/name-map)
(require 'elcomp/props)

;; FIXME - emacs must supply this value
(defconst elcomp--c-max-args 8)

(cl-defstruct elcomp--c
  decls
  decl-marker
  ;; Map symbols to their C names.
  interned-symbols
  ;; Map SSA names to their defining blocks.
  ;; This is a hack because we don't have a good out-of-ssa approach
  ;; yet.
  name-map
  (eh-count 0))

(defun elcomp--c-quote-string (str)
  "Quote a Lisp string according to C rules."
  (concat "\""
	  (mapconcat (lambda (c)
		       ;; Not really complete yet.
		       (cl-case c
			 ((?\\ ?\")
			  (string ?\\ c))
			 (?\n "\\n")
			 (?\r "\\r")
			 (?\t "\\t")
			 (t (string c))))
		     str "")
	  "\""))

(defun elcomp--c-name (symbol)
  "Compute the C name for a symbol."
  (replace-regexp-in-string "-" "_" (symbol-name symbol)))

(defun elcomp--c-intern-symbol (eltoc symbol)
  "Mark a symbol for init-time interning and return its name.
This is used for references to global symbols."
  (or (gethash symbol (elcomp--c-interned-symbols eltoc))
      ;; Use LQsym, not Qsym, to avoid clashes with things Emacs
      ;; defines itself.
      (puthash symbol (concat "LQ" (elcomp--c-name symbol))
	       (elcomp--c-interned-symbols eltoc))))

(defun elcomp--c-declare (eltoc sym)
  (unless (gethash sym (elcomp--c-decls eltoc))
    (save-excursion
      (goto-char (elcomp--c-decl-marker eltoc))
      (insert "  Lisp_Object " (elcomp--c-name sym) ";\n")
      (puthash sym t (elcomp--c-decls eltoc)))))

(defun elcomp--c-declare-handler (eltoc)
  (let ((name (format "h%d" (cl-incf (elcomp--c-eh-count eltoc)))))
    (save-excursion
      (goto-char (elcomp--c-decl-marker eltoc))
      (insert "  struct handler *" name ";\n")
      name)))

(defun elcomp--c-symbol (eltoc sym &optional no-declare)
  (unless no-declare
    (elcomp--c-declare eltoc sym))
  (insert (elcomp--c-name sym)))

;; FIXME - in emacs 25 this can be a generic.
(defun elcomp--c-emit-symref (eltoc insn)
  (cond
   ((symbolp insn)
    (insert (elcomp--c-intern-symbol eltoc insn)))
   ((elcomp--set-p insn)
    (elcomp--c-symbol eltoc (elcomp--sym insn)))
   ((elcomp--call-p insn)
    (elcomp--c-symbol eltoc (elcomp--sym insn)))
   ((elcomp--phi-p insn)
    ;; FIXME??
    (elcomp--c-symbol eltoc (elcomp--original-name insn)))
   ((elcomp--argument-p insn)
    (elcomp--c-symbol eltoc (elcomp--original-name insn) t))
   ((elcomp--constant-p insn)
    (let ((value (elcomp--value insn)))
      (cond
       ;; FIXME - in emacs 25 this can be a generic.
       ((symbolp value)
	(insert (elcomp--c-intern-symbol eltoc value)))
       ((integerp value)
	(insert "make_number (" (number-to-string value) ")"))
       ((stringp value)
	;; Could use make_string, but there's little point since GCC
	;; will optimize the strlen anyhow.
	(insert "build_string (" (elcomp--c-quote-string value) ")"))
       ((cl-typep value 'elcomp)
	(insert "K" (elcomp--c-name (elcomp--get-name value))))
       (t
	;: FIXME why does calling error here cause problems?
	;; Anyway this ought to emit some initialization code to
	;; construct non-primitve constants.
	;; (error "unhandled constant of type %S" (type-of value))
	(insert "BUG in elcomp--c-emit-symref")))))
   (t
    (error "unhandled case: %S" insn))))

(defun elcomp--c-emit-label (block)
  (insert (format "BB_%d" (elcomp--basic-block-number block))))

(cl-defgeneric elcomp--c-emit (insn _eltoc _bb)
  "FIXME"
  (error "unhandled case: %S" insn))

(cl-defmethod elcomp--c-emit ((insn elcomp--set) eltoc _bb)
  (elcomp--c-emit-symref eltoc insn)
  (insert " = ")
  (elcomp--c-emit-symref eltoc (elcomp--value insn)))

(defun elcomp--unbind-emitter (insn)
  "Emit a call to :elcomp-unbind.
This must be handled specially for now to avoid boxing the
argument."
  (let* ((args (elcomp--args insn))
	 (first-arg (car args)))
  (cl-assert (eq (length args) 1))
  (cl-assert (elcomp--constant-p first-arg))
  (let ((value (elcomp--value first-arg)))
    (cl-assert (integerp value))
    (insert "unbind_to (SPECPDL_INDEX - "
	    (number-to-string value)
	    ", Qnil)"))))

(defconst elcomp--c-direct-renames
  '((:elcomp-specbind . "specbind")
    (:elcomp-fetch-condition . "signal_value")
    (:save-excursion-save . "save_excursion_save")
    (:save-excursion-restore . "save_excursion_restore")
    (:save-restriction-save . "save_restriction_save")
    (:save-restriction-restore . "save_restriction_restore")
    (:unwind-protect-continue . "unwind_protect_continue")
    (:catch-value . "catch_value")
    (:pop-exception-handler . "pop_exception_handler")
    ;; FIXME
    (:ffi-call . "FFI_CALL")))

(defconst elcomp--c-numeric-comparisons '(> >= < <= =))

(defun elcomp--c-numeric-comparison-p (function args bb)
  (and (memq function elcomp--c-numeric-comparisons)
       (> (length args) 1)
       (cl-every (lambda (arg)
		   (let ((type (elcomp--look-up-type bb arg)))
		     (or (eq type 'integer)
			 (eq type 'float))))
		 args)))

(defun elcomp--c-numeric-comparison (function args eltoc bb)
  (let* ((operator (if (eq function '=)
		       "=="
		     (symbol-name function)))
	 (unwrapper (lambda (arg)
		      (if (elcomp--constant-p arg)
			  (insert (format "%s" (elcomp--value arg)))
			(insert
			 (if (eq (elcomp--look-up-type bb arg) 'integer)
			     "XINT"
			   "XFLOAT_DATA")
			 " (")
			(elcomp--c-emit-symref eltoc arg)
			(insert ")"))))
	 (prev (car args))
	 (first t))
    (insert "(")
    (dolist (arg (cdr args))
      (if first
	  (setf first nil)
	(insert " && "))
      (funcall unwrapper prev)
      (insert " " operator " ")
      (funcall unwrapper arg)
      (setf prev arg))
    (insert ") ? Qt : Qnil")))

(defun elcomp--c-unary-numeric-op-p (function args bb)
  (and (memq function '(1+ 1- -))
       (eq (length args) 1)
       (cl-every (lambda (arg)
		   (let ((type (elcomp--look-up-type bb arg)))
		     (or (eq type 'integer)
			 (eq type 'float))))
		 args)))

(defun elcomp--c-unary-numeric-op (function args eltoc bb)
  (cl-assert (eq (length args) 1))
  (let* ((arg (car args))
	 (type (elcomp--look-up-type bb arg))
	 (maker (if (eq type 'integer)
		    "make_number"
		  "make_float"))
	 (unwrapper (if (eq type 'integer)
		    "XINT"
		    "XFLOAT_DATA")))
    (cl-case function
      ((1+)
       (insert maker " (" unwrapper " (")
       (elcomp--c-emit-symref eltoc arg)
       (insert ") + 1)"))
      ((1-)
       (insert maker " (" unwrapper " (")
       (elcomp--c-emit-symref eltoc arg)
       (insert ") - 1)"))
      ((-)
       (insert maker " (-" unwrapper " (")
       (elcomp--c-emit-symref eltoc arg)
       (insert "))"))
      (t
       (error "whoops %S" function)))))

(cl-defmethod elcomp--c-emit ((insn elcomp--call) eltoc bb)
  (when (elcomp--sym insn)
    (elcomp--c-emit-symref eltoc insn)
    (insert " = "))
  (cond
   ((eq (elcomp--func insn) :elcomp-unbind)
    (elcomp--unbind-emitter insn))

   ((elcomp--c-numeric-comparison-p (elcomp--func insn) (elcomp--args insn) bb)
    (elcomp--c-numeric-comparison (elcomp--func insn) (elcomp--args insn)
				  eltoc bb))

   ((elcomp--c-unary-numeric-op-p (elcomp--func insn) (elcomp--args insn) bb)
    (elcomp--c-unary-numeric-op (elcomp--func insn) (elcomp--args insn)
				eltoc bb))

   (t
    (let* ((function
	    (or	(elcomp--c-opt insn
			       (mapcar (lambda (arg)
					 (elcomp--look-up-type bb arg))
				       (elcomp--args insn)))
		(elcomp--func insn)))
	   (arg-list (elcomp--args insn))
	   (is-direct (elcomp--func-direct-p function))
	   (is-vararg nil))
      (cond
       ((stringp function)	     ; Was optimized by elcomp--c-opt.
	(insert function " ("))
       ((keywordp function)
	(insert (or (cdr (assq function elcomp--c-direct-renames))
		    (format "BUG«%S»" function))
		" (")
	;; FIXME hack
	(when (memq function '(:catch-value :elcomp-fetch-condition
					    :unwind-protect-continue))
	  (insert "&prev_handler")))
       (is-direct
	(if-let ((rename (assq function elcomp--c-renames)))
	    (insert (cdr rename) " (")
	  (insert "F" (elcomp--c-name function) " ("))
	(when (and (symbolp function)
		   (fboundp function)
		   (subrp (symbol-function function))
		   (eq (cdr (subr-arity (symbol-function function))) 'many))
	  (insert (format "%d, ((Lisp_Object[]) { " (length arg-list)))
	  (setf is-vararg t)))
       (t
	(push function arg-list)
	;; FIXME - what if not a symbol, etc.
	(setf is-vararg t)
	(insert (format "Ffuncall (%d, ((Lisp_Object[]) { "
			(length arg-list)))))
      (let ((first t))
	(dolist (arg arg-list)
	  (if first
	      (setf first nil)
	    (insert ", "))
	  (elcomp--c-emit-symref eltoc arg)))
      (if is-vararg
	  (insert " }))")
	(insert ")"))))))

(defun elcomp--c-set-phis-on-entry (eltoc this-bb target-bb)
  (maphash
   (lambda (_name phi)
     (insert "      ")
     (elcomp--c-emit-symref eltoc phi)
     (insert " = ")
     (cl-block done
       ;; This algorithm sucks.
       (let ((check-bb this-bb))
	 (while t
	   (maphash
	    (lambda (arg _ignore)
	      (when (eq (gethash arg (elcomp--c-name-map eltoc)) check-bb)
		(elcomp--c-emit-symref eltoc arg)
		(cl-return-from done)))
	    (elcomp--args phi))
	   (when (eq check-bb
		     (elcomp--basic-block-immediate-dominator check-bb))
	     (cl-return-from done))
	   (setf check-bb
		 (elcomp--basic-block-immediate-dominator check-bb)))))
     (insert ";\n"))
   (elcomp--basic-block-phis target-bb)))

(cl-defmethod elcomp--c-emit ((insn elcomp--goto) eltoc bb)
  (elcomp--c-set-phis-on-entry eltoc bb (elcomp--block insn))
  (insert "  goto ")
  (elcomp--c-emit-label (elcomp--block insn)))

(cl-defmethod elcomp--c-emit ((insn elcomp--if) eltoc bb)
  (insert "if (!NILP (")
  (elcomp--c-emit-symref eltoc (elcomp--sym insn))
  (insert "))\n")
  (insert "    {\n")
  (elcomp--c-set-phis-on-entry eltoc bb (elcomp--block-true insn))
  (insert "      goto ")
  (elcomp--c-emit-label (elcomp--block-true insn))
  (insert ";\n")
  (insert "    }\n")
  (insert "  else\n")
  (insert "    {\n")
  (elcomp--c-set-phis-on-entry eltoc bb (elcomp--block-false insn))
  (insert "      goto ")
  (elcomp--c-emit-label (elcomp--block-false insn))
  (insert ";\n")
  (insert "    }"))

(cl-defmethod elcomp--c-emit ((insn elcomp--return) eltoc _bb)
  (insert "return ")
  (elcomp--c-emit-symref eltoc (elcomp--sym insn)))

(cl-defmethod elcomp--c-emit ((insn elcomp--catch) eltoc _bb)
  (let ((name (elcomp--c-declare-handler eltoc)))
    (insert "  " name " = push_handler (")
    (elcomp--c-emit-symref eltoc (elcomp--tag insn))
    (insert ", CATCHER);\n")
    (insert "  if (sys_setjmp (" name "->jmp))\n")
    (insert "    {\n")
    (insert "      eassert (handlerlist == " name ");\n")
    (insert "      exit_exception_handler ();\n")
    (insert "      goto ")
    (elcomp--c-emit-label (elcomp--handler insn))
    (insert ";\n")
    (insert "    }\n")))

(cl-defmethod elcomp--c-emit ((_insn elcomp--condition-case) _eltoc _bb)
  ;; This one is handled specially for efficiency.
  (error "should not be called"))

(cl-defmethod elcomp--c-emit ((insn elcomp--unwind-protect) eltoc _bb)
  (let ((name (elcomp--c-declare-handler eltoc)))
    ;; Emacs doesn't actually have anything for this yet.
    (insert "  " name " = push_handler (Qnil, CATCHER_ALL);\n")
    (insert "  if (sys_setjmp (" name "->jmp))\n")
    (insert "    {\n")
    (insert "      eassert (handlerlist == " name ");\n")
    (insert "      exit_exception_handler ();\n")
    (insert "      goto ")
    (elcomp--c-emit-label (elcomp--handler insn))
    (insert ";\n")
    (insert "    }\n")))

(cl-defmethod elcomp--c-emit ((_insn elcomp--fake-unwind-protect) _eltoc _bb)
  ;; Nothing.
  )

(defun elcomp--c-emit-condition-case (eltoc eh-from eh-to)
  (let ((name (elcomp--c-declare-handler eltoc)))
    ;; The magic Qt means to stop on all conditions; see
    ;; eval.c:find_handler_clause.
    (insert "  " name " = push_handler (Qt, CONDITION_CASE);\n")
    (insert "  if (sys_setjmp (" name "->jmp))\n")
    (insert "    {\n")
    (insert "      eassert (handlerlist == " name ");\n")
    (insert "      exit_exception_handler ();\n")
    (while (and (not (eq eh-from eh-to))
		(elcomp--condition-case-p (car eh-from)))
      (insert "      if (!NILP (find_handler_clause (")
      (elcomp--c-emit-symref eltoc (elcomp--condition-name (car eh-from)))
      (insert ", signal_conditions (&prev_handler))))\n")
      (insert "        goto ")
      (elcomp--c-emit-label (elcomp--handler (car eh-from)))
      (insert ";\n")
      (setf eh-from (cdr eh-from)))
    (insert "    }\n"))
  eh-from)

(defun elcomp--c-first-parent (block)
  (elcomp--any-hash-key (elcomp--basic-block-parents block)))

(defun elcomp--c-emit-exceptions (eltoc block)
  (let* ((first-parent (elcomp--c-first-parent block))
	 (parent-eh (if first-parent
			(elcomp--basic-block-exceptions first-parent)
		      ;; No parent means it is the first block.
		      nil)))
    (let ((bb-eh (elcomp--basic-block-exceptions block)))
      (when (and (not (or (memq (car bb-eh) parent-eh)
			  (and parent-eh (not bb-eh))))
		 bb-eh)
	;; If our first exception does not appear in the parent
	;; list, then we have to push at least one.
	(while (and bb-eh (not (eq bb-eh parent-eh)))
	  (if (elcomp--condition-case-p (car bb-eh))
	      (setf bb-eh (elcomp--c-emit-condition-case eltoc bb-eh
							 parent-eh))
	    (elcomp--c-emit (car bb-eh) eltoc block)
	    (setf bb-eh (cdr bb-eh))))))))

(defun elcomp--c-emit-block (eltoc bb)
  (elcomp--c-emit-label bb)
  (insert ":\n")
  (elcomp--c-emit-exceptions eltoc bb)
  (dolist (insn (elcomp--basic-block-code bb))
    (insert "  ")
    (elcomp--c-emit insn eltoc bb)
    (insert ";\n")))

(defun elcomp--c-parse-args (arg-list)
  (let ((min-args 0))
    (while (and arg-list (not (memq (car arg-list) '(&optional &rest))))
      (pop arg-list)
      (cl-incf min-args))
    (let ((max-args min-args))
      (while (eq (car arg-list) '&optional)
	(pop arg-list)
	(pop arg-list)
	(cl-incf max-args))
      (if (or (eq (car arg-list) '&rest)
	      (> max-args elcomp--c-max-args))
	  (cons min-args "MANY")
	(cons min-args max-args)))))

(defun elcomp--c-generate-defun (compiler)
  (let* ((info (elcomp--defun compiler))
	 (sym (elcomp--get-name compiler))
	 (c-name (elcomp--c-name sym))
	 (arg-info (elcomp--c-parse-args (cadr info))))
    (insert
     (format "DEFUN (%s, F%s, S%s, %s, %s,\n    %s,\n    doc: /* %s */)\n"
	     (elcomp--c-quote-string (symbol-name sym))
	     c-name c-name
	     (car arg-info) (cdr arg-info)
	     ;; Interactive.
	     ;; FIXME: quoting for the interactive spec
	     ;; Note that we can have a whole lisp form here.
	     (or (nth 3 info) "0")
	     ;; Doc string.  FIXME - comment quoting
	     (or (nth 2 info) "nothing??"))) ;FIXME anything?
    (if (equal (cdr arg-info) "MANY")
	(let ((nargs (elcomp--c-name (cl-gensym "nargs")))
	      (args (elcomp--c-name (cl-gensym "args"))))
	  (insert "  (ptrdiff_t " nargs ", Lisp_Object *" args ")\n{\n")
	  ;; We need special parsing for &rest arguments or when the
	  ;; number of format arguments is greater than the maximum.
	  ;; First emit the declarations.
	  (dolist (arg (cadr info))
	    (unless (memq arg '(&optional &rest))
	      (insert "  Lisp_Object " (symbol-name arg) " = Qnil;\n")))
	  ;; Now initialize each one.
	  (let ((is-rest nil))
	    (dolist (arg (cadr info))
	      (cond
	       ((eq arg '&rest)
		(setf is-rest t))
	       ((eq arg '&optional)
		;; Nothing.
		)
	       (t
		(if is-rest
		    (insert "  " (symbol-name arg) " = Flist ("
			    nargs ", " args ");\n")
		  (insert "  if (" nargs " > 0)\n"
			  "    {\n"
			  "      " (symbol-name arg) " = *" args "++;\n"
			  "      --" nargs ";\n"
			  "    }\n")))))))
      (insert "  (")
      (let ((first t))
	(dolist (arg (cadr info))
	  (unless (eq arg '&optional)
	    (unless first
	      (insert ", "))
	    (setf first nil)
	    (insert "Lisp_Object " (symbol-name arg)))))
      (insert ")\n{\n"))))

(defun elcomp--c-translate-one (compiler symbol-hash)
  (elcomp--require-back-edges compiler)
  (elcomp--compute-dominators compiler)
  (let ((eltoc (make-elcomp--c :decls (make-hash-table)
			       :decl-marker (make-marker)
			       :interned-symbols symbol-hash
			       :name-map (elcomp--make-name-map compiler))))
    (elcomp--c-generate-defun compiler)
    ;; This approach is pretty hacky.
    (insert "  struct handler prev_handler;\n")
    (set-marker (elcomp--c-decl-marker eltoc) (point))
    (insert "\n")
    (set-marker-insertion-type (elcomp--c-decl-marker eltoc) t)
    (elcomp--iterate-over-bbs compiler
			      (lambda (bb)
				(elcomp--c-emit-block eltoc bb)))
    (insert "}\n\n")
    (set-marker (elcomp--c-decl-marker eltoc) nil)))

;; If BASE-FILENAME is nil, a module-like file is generated.  (But of
;; course this doesn't work since modules use a JNI-like thing.)
;; Otherwise, the generated code looks more like the Emacs internals.
(defun elcomp--c-translate (unit &optional base-filename)
  (let ((symbol-hash (make-hash-table)))
    (maphash
     (lambda (_ignore compiler)
       (elcomp--c-translate-one compiler symbol-hash))
     (elcomp--compilation-unit-defuns unit))
    ;; Define the symbol variables.
    (save-excursion
      (goto-char (point-min))
      (insert "#include <config.h>\n"
	      "#include <lisp.h>\n\n"
	      ;; FIXME this is less efficient than it could be.
	      ;; We only need a couple of fields from this.
	      "#define exit_exception_handler() (prev_handler = *handlerlist, handlerlist = handlerlist->next)\n"
	      "#define pop_exception_handler() handlerlist = handlerlist->next\n"
	      "#define catch_value(H) ((H)->val)\n"
	      "#define signal_conditions(H) (XCAR ((H)->val))\n"
	      "#define signal_value(H) (XCDR ((H)->val))\n\n")
      (unless base-filename
	(insert "int plugin_is_GPL_compatible;\n\n"))
      (maphash (lambda (_symbol c-name)
		 (insert "static Lisp_Object " c-name ";\n"))
	       symbol-hash)
      (insert "\n")

      ;; Create "K" values that are are tagged SUBR values for all the
      ;; functions.
      (maphash
       (lambda (_ignore compiler)
	 (let ((name (elcomp--get-name compiler)))
	   (insert "static Lisp_Object K" (elcomp--c-name name) ";\n")))
       (elcomp--compilation-unit-defuns unit))
      (insert "\n"))
    (insert "\n"
	    "void\n"
	    (if base-filename
		(concat "syms_of_" base-filename)
	      "init")
	    " (void)\n{\n")
    ;; Intern all the symbols we refer to.
    (maphash (lambda (symbol c-name)
	       (insert "  " c-name " = intern_c_string (" 
		       (elcomp--c-quote-string (symbol-name symbol))
		       ");\n")
	       (insert "  staticpro (&" c-name ");\n"))
	     symbol-hash)
    (insert "\n")
    ;; Register our exported functions with Lisp.
    (maphash
     (lambda (_ignore compiler)
       (let ((name (car (elcomp--defun compiler))))
	 (if name
	     (insert "  defsubr (&S" (elcomp--c-name name) ");\n")
	   (insert "  XSETPVECTYPE (&S"
		   (symbol-name (elcomp--get-name compiler))
		   ", PVEC_SUBR);\n"))
	 (insert "  XSETSUBR (K" (elcomp--c-name (elcomp--get-name compiler))
		 ", &S" (elcomp--c-name (elcomp--get-name compiler))
		 ");\n")))
     (elcomp--compilation-unit-defuns unit))
    (insert "}\n")))

(provide 'elcomp/eltoc)

;;; eltoc.el ends here
