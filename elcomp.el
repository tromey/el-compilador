;; TO DO:

;; Must respect special-variable-p
;; for now just reject it
;; when we handle these the expansion of 'let' must be
;; trickier

;; Jump threading
;; rework how labels are stored

;; Ideas:
;; * always inline mapcar &c

;; * Why doesn't mapconcat let the lambda return chars?

;; We need a macro for 'declare' that expands
;; so we can see types.
;; The plan is to use the type to change the type in the C code.
;; So:  (let ((i value)) (declare (type fixnum i)) ...)
;; If VALUE is a lisp object this will expand to
;;    EMACS_INT i = XINT (value)
;; If VALUE is a fixnum it will be simply:
;;    EMACS_INT i = value;

;; We should also allow a declaration that allows a direct C
;; call, not allowing symbol redefinition.
;; (declare (direct FUNC))

;; Notes
;; * I wonder if progv is implemented correctly now that
;;   macroexpand is done eagerly

(cl-defstruct (elcomp (:conc-name elcomp--))
  ;; An alist holding symbol rewrites.  The car of each element is a
  ;; symbol in the original code.  The cdr is the symbol to which it
  ;; is rewritten.
  rewrite-alist
  ;; A list of symbols representing variables that must be defined in
  ;; the generated code.
  variables
  ;; Next label value.
  (next-label 0) 
  ;; Linearized code.
  code
  ;; Last link of linearized code.
  code-link
  ;; The current defun being compiled.
  ;; This is a list (NAME ARGLIST DOC INTERACTIVE).
  defun
  ;; A list of all defun symbols being compiled.
  defuns)

(cl-defstruct elcomp--basic-block
  ;; Block number.
  number
  ;; Instructions.
  insns)

(defun elcomp--declare (&rest specs)
  (cons 'declare specs))

(defun elcomp--new-var (compiler &optional symname)
  (let* ((cell (and symname
		   (memq symname (elcomp--rewrite-alist compiler))))
	 (result (if cell
		     (cl-gensym)
		   (or symname
		       (cl-gensym)))))
    (push result (elcomp--variables compiler))
    (put result :elcomp t)
    ;; (put result :original FIXME)
    result))

(defun elcomp--rewrite-one-ref (compiler ref)
  "Rewrite REF.
REF can be a symbol, in which case it is rewritten following
`elcomp--rewrite-alist' and returned.
Or REF can be a constant, in which case it is returned unchanged."
  (if (symbolp ref)
      (progn
	;; Temporary.
	(if (and (special-variable-p ref)
		 (not (memq ref '(nil t))))
	    (error "special variables not supported yet: %s" ref))))
  (let ((tem (assq ref (elcomp--rewrite-alist compiler))))
    (if tem
	(cdr tem)
      ref)))

(defun elcomp--label (compiler)
  (prog1
      (elcomp--next-label compiler)
    (cl-incf (elcomp--next-label compiler))))

(defun elcomp--set-type (var type-name)
  (let ((found-type (get var :type)))
    (if found-type
	(if (not (eq found-type type-name))
	    (error "variable already has a type"))))
  (put var :type type-name))

;; FIXME: should do type promotion.
(defun elcomp--any-argument-typed-p (args)
  "Returns the type."
  (let ((found-type nil))
    (while args
      (if (and (symbolp (car args))
	       (memq (get (car args) :type) '(float fixnum)))
	  (progn
	    (setf found-type (get (car args) :type))
	    (setf args nil))
	(setf args (cdr args))))
    found-type))

(defun elcomp--append (compiler code result-location &rest refs)
  "Append ARGS to the linearized code."
  (let ((args (list (apply #'list code result-location refs))))
    (if (elcomp--code-link compiler)
	(setcdr (elcomp--code-link compiler) args)
      (setf (elcomp--code compiler) args))
    (setf (elcomp--code-link compiler) args)
    ;; Type promotion.
    (if result-location
	(cond
	 ((and (eq code 'set)
	       (symbolp (car refs)))
	  (let ((found-type (get (car refs) :type)))
	    (if found-type
		(elcomp--set-type result-location found-type))))
	 ((eq code 'call)
	  (let ((found-type (elcomp--any-argument-typed-p refs)))
	    (if found-type
		(elcomp--set-type result-location found-type))))))))

(defconst elcomp--simple-math
  '(< <= > >= /= + - * / 1+))

(defun elcomp--check-simple-math (form)
  (and (eq (car form) 'call)
       (memq (nth 2 form) elcomp--simple-math)
       (let ((found-type (elcomp--any-argument-typed-p (nthcdr 3 form))))
	 (if found-type
	     (dolist (arg (nthcdr 3 form))
	       (if (symbolp arg)
		   (elcomp--set-type arg found-type)))))))

(defun elcomp--linearize-body (compiler body result-location
					&optional result-index)
  (let ((i 1))
    (while body
      (elcomp--linearize compiler (car body)
			 (if (or (eq i result-index)
				 (and (eq result-index nil)
				      (not (cdr body))))
			     result-location
			   nil))
      (setf body (cdr body))
      (cl-incf i))))

;; (defun elcomp--handler-name (name)
;;   (intern (concat "elcomp--compiler--" (symbol-name name))))

;; (defmacro define-elcomp-handler (name arg-list &rest body)
;;   `(defun ,(elcomp--handler-name name) arg-list body))

(defun elcomp--linearize (compiler form result-location)
  "Linearize FORM and return the result.

Linearization turns a form from an ordinary Lisp form into
a sequence of simple objects.  Each object is of one of
the forms:

   (set LOC RESULT)
   (call SYM FUNC [ARG]...)
   (label N)
   (goto N)
   (if SYM [N-TRUE | nil] [N-FALSE])
"
  (if (not (consp form))
      (if result-location
	  (elcomp--append compiler 'set result-location
			 (elcomp--rewrite-one-ref compiler form)))
    (let ((fn (car form)))
      (cond
       ((eq fn 'quote)
	(if result-location
	    (elcomp--append compiler 'set result-location form)))
       ((eq 'lambda (car-safe fn))
	(error "lambda not supported"))
       ((eq fn 'let)
	;; Arrange to reset the rewriting table outside the 'let'.
	(cl-letf (((elcomp--rewrite-alist compiler)
		   (elcomp--rewrite-alist compiler))
		  (let-symbols nil))
	  ;; Compute the values.
	  (dolist (sexp (cadr form))
	    (let* ((sym (if (symbolp sexp)
			    sexp
			  (car sexp)))
		   (sym-initializer (if (consp sexp)
					(cadr sexp)
				      nil))
		   (sym-result (elcomp--new-var compiler sym)))
	      ;; If there is a body, compute it.
	      (elcomp--linearize compiler sym-initializer sym-result)
	      (push (cons sym sym-result) let-symbols)))
	  ;; Push the new values onto the rewrite list.
	  (setf (elcomp--rewrite-alist compiler)
		(append let-symbols (elcomp--rewrite-alist compiler)))
	  ;; Now evaluate the body of the let.
	  (elcomp--linearize-body compiler (cddr form) result-location)))
       ((eq fn 'let*)
	;; Arrange to reset the rewriting table outside the 'let*'.
	(cl-letf (((elcomp--rewrite-alist compiler)
		   (elcomp--rewrite-alist compiler)))
	  ;; Compute the values.
	  (dolist (sexp (cadr form))
	    (let* ((sym (if (symbolp sexp)
			    sexp
			  (car sexp)))
		   (sym-initializer (cadr sexp))
		   (sym-result (elcomp--new-var compiler sym)))
	      ;; If there is a body, compute it.
	      (elcomp--linearize compiler sym-initializer sym-result)
	      (push (cons sym sym-result) (elcomp--rewrite-alist compiler))))
	  ;; Now evaluate the body of the let*.
	  (elcomp--linearize-body compiler (cddr form) result-location)))

       ;; Do we need set?  or what else?  setq-default?  set-default?
       ((eq fn 'setq)
	(setf form (cdr form))
	(let ((last-rewritten-sym nil))
	  (while form
	    (let* ((sym (car form))
		   (rewritten-sym (elcomp--rewrite-one-ref compiler sym))
		   (val (cadr form)))
	      (setf last-rewritten-sym rewritten-sym)
	      (elcomp--linearize compiler val rewritten-sym))
	    (setf form (cddr form)))
	  ;; Return the value.
	  (if result-location
	      (elcomp--append compiler
			      'set result-location last-rewritten-sym))))

       ((eq fn 'cond)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (clause (cdr form))
	    (let ((this-cond-var (if (cdr clause)
				     (elcomp--new-var compiler)
				   result-location))
		  (next-label (elcomp--label compiler)))
	      ;; Emit the condition.
	      (elcomp--linearize compiler (car clause) this-cond-var)
	      ;; The test.
	      (elcomp--append compiler 'if this-cond-var nil next-label)
	      ;; The body.
	      (if (cdr clause)
		  (elcomp--linearize-body compiler
					  (cdr clause) result-location))
	      ;; Done.
	      ;; We could emit this goto for the last clause,
	      ;; but the C compiler will zap it anyway, so we
	      ;; are just lazy here.
	      (elcomp--append compiler 'goto label-done)
	      (elcomp--append compiler 'label next-label)))
	  (elcomp--append compiler 'label label-done)))

       ((eq fn 'progn)
	(elcomp--linearize-body compiler (cdr form) result-location))
       ((eq fn 'prog1)
	(elcomp--linearize-body compiler (cdr form) result-location 1))
       ((eq fn 'prog2)
	(elcomp--linearize-body compiler (cdr form) result-location 2))

       ((memq fn '(save-excursion save-restriction save-current-buffer))
	(error "not supported")
	)

       ((eq fn 'with-output-to-temp-buffer)
	(error "not supported")
	)

       ((eq fn 'while)
	(let ((label-top (elcomp--label compiler))
	      (label-done (elcomp--label compiler))
	      (cond-var (elcomp--new-var compiler)))
	  (if result-location
	      (elcomp--append compiler 'set result-location 'nil))
	  ;; FIXME: set the result-location
	  (elcomp--append compiler 'label label-top)
	  ;; The condition expression and goto.
	  (elcomp--linearize compiler (cadr form) cond-var)
	  (elcomp--append compiler 'if cond-var nil label-done)
	  ;; The body.
	  (elcomp--linearize-body compiler (cddr form) nil)
	  (elcomp--append compiler 'goto label-top)
	  (elcomp--append compiler 'label label-done)))

       ((eq fn 'if)
	(let ((label-false (elcomp--label compiler))
	      (label-done (elcomp--label compiler))
	      (cond-var (elcomp--new-var compiler)))
	  ;; The condition expression and goto.
	  (elcomp--linearize compiler (cadr form) cond-var)
	  (elcomp--append compiler 'if cond-var nil label-false)
	  ;; The true branch.
	  (elcomp--linearize compiler (caddr form) result-location)
	  ;; The end of the true branch.
	  (elcomp--append compiler 'goto label-done)
	  ;; The false branch.
	  (elcomp--append compiler 'label label-false)
	  (if (cdddr form)
	      (elcomp--linearize-body compiler (cdddr form) result-location))
	  ;; The end of the statement.
	  (elcomp--append compiler 'label label-done)))

       ((eq fn 'and)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (condition (cdr form))
	    (elcomp--linearize compiler condition result-location)
	    ;; FIXME: don't need this "if" for the last iteration.
	    ;; FIXME: "and" in conditionals could be handled better.
	    (elcomp--append compiler 'if result-location label-done nil))
	  (elcomp--append compiler 'label label-done)))

       ((eq fn 'or)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (condition (cdr form))
	    (elcomp--linearize compiler condition result-location)
	    (elcomp--append compiler 'if result-location label-done nil))
	  (elcomp--append compiler 'label label-done)))

       ((eq fn 'interactive)
	nil)

       ((memq fn '(function condition-case))
	(error "not supported"))

       ((eq fn 'unwind-protect)
	(error "not supported"))

       ((eq fn 'catch)
	(error "not supported"))

       ;; Needed as long as we run byte-optimize-form after cconv.
       ((eq fn 'internal-make-closure)
	(error "not supported"))

       ((eq fn 'declare)
	(dolist (spec (cdr form))
	  ;; FIXME this should also examine direct-calls
	  (pcase spec
	      (`(type ,type-name . ,variables)
	       (dolist (var variables)
		 (setf var (elcomp--rewrite-one-ref compiler var))
		 (elcomp--set-type var type-name))))))

       ((not (symbolp fn))
	;; FIXME - lambda or the like
	)

       (t
	(if (special-form-p (symbol-function fn))
	    (error "unhandled special form"))
	;; An ordinary function call.
	(let ((these-args
	       ;; Compute each argument.
	       (mapcar (lambda (arg)
			 (if (or (numberp arg)
				 (and (symbolp arg)
				      (get arg :elcomp)))
			     arg
			   (let ((one-arg (elcomp--new-var compiler)))
			     (elcomp--linearize compiler arg one-arg)
			     one-arg)))
		       (cdr form))))
	  ;; Make the call.
	  (apply #'elcomp--append compiler
		 'call result-location fn these-args)))
       ))))

(defun elcomp--extract-defun (compiler form)
  (unless (eq 'defun (car form))
    (error "not a defun"))
  (push (cadr form) (elcomp--defuns compiler))
  (setf (elcomp--defun compiler)
	(list (cadr form) (caddr form)))
  (setf form (cdddr form))
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
	      (nconc (elcomp--defun compiler) (list (cadar form))))
	(setf form (cdr form)))
    (setf (elcomp--defun compiler) (nconc (elcomp--defun compiler) nil)))
  (cons 'progn form))

(defun elcomp--translate (form buffer)
  (byte-compile-close-variables
   (let* ((byte-compile-macro-environment
	   (cons '(declare . elcomp--declare)
		 byte-compile-macro-environment))
	  (compiler (make-elcomp))
	  (result-var (elcomp--new-var compiler))
	  (code nil))
     (setf form (elcomp--extract-defun compiler form))
     (elcomp--linearize compiler
      (byte-optimize-form (macroexpand-all form
				       byte-compile-macro-environment))
      result-var)
     compiler)))

;; Temporary function for hacking.
(defun elcomp--do (form)
  (let ((buf (get-buffer-create "*ELCOMP*")))
    (with-current-buffer buf
      (erase-buffer)
      (pp (elcomp--code (elcomp--translate form buf)) (current-buffer)))
    (pop-to-buffer buf)))
