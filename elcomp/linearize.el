;;; linearize.el --- linearize lisp forms.  -*- lexical-binding:t -*-

;;; Commentary:

;; Turn Emacs Lisp forms into compiler objects.

;;; Code:

(require 'elcomp)

(defun elcomp--push-fake-unwind-protect (compiler num)
  (let* ((first-exception (car (elcomp--exceptions compiler)))
	 (new-exception
	  (if (elcomp--fake-unwind-protect-p first-exception)
	      (progn
		(pop (elcomp--exceptions compiler))
		(elcomp--fake-unwind-protect
		 "fake" :count (+ (oref first-exception :count) num)))
	    (elcomp--fake-unwind-protect "fake" :count num))))
    (push new-exception (elcomp--exceptions compiler)))
  (elcomp--make-block-current compiler (elcomp--label compiler)))

(defun elcomp--pop-fake-unwind-protects (compiler num)
  (let* ((first-exception (pop (elcomp--exceptions compiler))))
    (cl-assert (elcomp--fake-unwind-protect-p first-exception))
    (cl-assert (>= (oref first-exception :count) num))
    (if (> (oref first-exception :count) num)
	(push (elcomp--fake-unwind-protect
	       "fake" :count (- (oref first-exception :count) num)))))
  (elcomp--make-block-current compiler (elcomp--label compiler)))

(defun elcomp--new-var (compiler &optional symname)
  (let* ((cell (memq symname (elcomp--rewrite-alist compiler))))
    (if cell
	(cl-gensym)
      (or symname
	  (cl-gensym)))))

(defun elcomp--rewrite-one-ref (compiler ref)
  "Rewrite REF.
REF can be a symbol, in which case it is rewritten following
`elcomp--rewrite-alist' and returned.
Or REF can be a constant, in which case it is returned unchanged."
  (if (elcomp--constant-child-p ref)
      ref
    ;; Temporary.
    (if (and (special-variable-p ref)
	     (not (memq ref '(nil t))))
	(error "special variables not supported yet: %s" ref))
    (let ((tem (assq ref (elcomp--rewrite-alist compiler))))
      (if tem
	  (cdr tem)
	ref))))

(defun elcomp--label (compiler)
  (prog1
      (make-elcomp--basic-block :number (elcomp--next-label compiler)
				:exceptions (elcomp--exceptions compiler))
    (cl-incf (elcomp--next-label compiler))))

(defun elcomp--add-to-basic-block (block obj)
  (let ((new-cell (cons obj nil)))
    (if (elcomp--basic-block-code-link block)
	(setf (cdr (elcomp--basic-block-code-link block)) new-cell)
      (setf (elcomp--basic-block-code block) new-cell))
    (setf (elcomp--basic-block-code-link block) new-cell)))

(defun elcomp--add-basic (compiler obj)
  (elcomp--add-to-basic-block (elcomp--current-block compiler) obj))

(defun elcomp--add-set (compiler sym value)
  (elcomp--add-basic compiler (elcomp--set "set" :sym sym :value value)))

(defun elcomp--add-call (compiler sym func args)
  (if (and (symbolp func)
	   (elcomp--func-noreturn-p func))
      (progn
	;; Add the terminator instruction and push a new basic block
	;; -- this block will be discarded later, but that's ok.  Also
	;; discard the assignment.
	(elcomp--add-basic compiler
			   (elcomp--diediedie "diediedie" :sym nil :func func
					      :args args))
	(setf (elcomp--current-block compiler) (elcomp--label compiler)))
    (elcomp--add-basic compiler (elcomp--call "call" :sym sym :func func
					      :args args))))

(defun elcomp--add-return (compiler sym)
  (elcomp--add-basic compiler (elcomp--return "return" :sym sym)))

(defun elcomp--add-goto (compiler block)
  (elcomp--add-basic compiler (elcomp--goto "goto" :block block))
  ;; Push a new block.
  (setf (elcomp--current-block compiler) (elcomp--label compiler)))

(defun elcomp--add-if (compiler sym block-true block-false)
  (cl-assert (or block-true block-false))
  (let ((next-block))
    (unless block-true
      (setf block-true (elcomp--label compiler))
      (setf next-block block-true))
    (unless block-false
      (setf block-false (elcomp--label compiler))
      (setf next-block block-false))
    (elcomp--add-basic compiler (elcomp--if "if"
					    :sym sym
					    :block-true block-true
					    :block-false block-false))
    ;; Push a new block.
    (setf (elcomp--current-block compiler) next-block)))

(defun elcomp--last-instruction (block)
  (car (elcomp--basic-block-code-link block)))

(gv-define-setter elcomp--last-instruction (val block)
  `(setcar (elcomp--basic-block-code-link ,block) ,val))

(defun elcomp--first-instruction (block)
  (car (elcomp--basic-block-code block)))

(gv-define-setter elcomp--first-instruction (val block)
  `(setcar (elcomp--basic-block-code ,block) ,val))

(defun elcomp--nonreturn-terminator-p (obj)
  (or (elcomp--goto-child-p obj)
      (elcomp--if-child-p obj)))

(defun elcomp--terminator-p (obj)
  (or (elcomp--goto-child-p obj)
      (elcomp--if-child-p obj)
      (elcomp--return-child-p obj)
      (elcomp--diediedie-child-p obj)))

(defun elcomp--invalidate-cfg (compiler)
  (elcomp--invalidate-back-edges compiler))

(defun elcomp--variable-p (obj)
  "Return t if OBJ is a variable when linearizing.
A variable is a symbol that is not a keyword."
  (and (symbolp obj)
       (not (keywordp obj))
       (not (memq obj '(t nil)))))

(defun elcomp--make-block-current (compiler block)
  ;; Terminate the previous basic block.
  (let ((insn (elcomp--last-instruction (elcomp--current-block compiler))))
    (if (not (elcomp--terminator-p insn))
	(elcomp--add-basic compiler (elcomp--goto "goto" :block block)))
    (setf (elcomp--current-block compiler) block)))

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

(defun elcomp--operand (compiler form)
  (cond
   ((elcomp--variable-p form)
    (elcomp--rewrite-one-ref compiler form))
   ((atom form)
    (elcomp--constant "constant" :value form))
   ((eq (car form) 'quote)
    (elcomp--constant "constant" :value (cadr form)))
   (t
    (let ((var (elcomp--new-var compiler)))
      (elcomp--linearize compiler form var)
      var))))
 
(defun elcomp--linearize (compiler form result-location)
  "Linearize FORM and return the result.

Linearization turns a form from an ordinary Lisp form into a
sequence of objects.  FIXME ref the class docs"
  (if (atom form)
      (if result-location
	  (elcomp--add-set compiler result-location
			   (elcomp--operand compiler form)))
    (let ((fn (car form)))
      (cond
       ((eq fn 'quote)
	(if result-location
	    (elcomp--add-set compiler result-location
			     (elcomp--operand compiler form))))
       ((eq 'lambda (car-safe fn))
	(error "lambda not supported"))
       ((eq fn 'let)
	;; Arrange to reset the rewriting table outside the 'let'.
	(cl-letf (((elcomp--rewrite-alist compiler)
		   (elcomp--rewrite-alist compiler))
		  (let-symbols nil)
		  (spec-vars nil))
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
	      (if (special-variable-p sym)
		  (push (cons sym sym-result) spec-vars)
		(push (cons sym sym-result) let-symbols))))
	  ;; Push the new values onto the rewrite list.
	  (setf (elcomp--rewrite-alist compiler)
		(nconc let-symbols (elcomp--rewrite-alist compiler)))
	  (when spec-vars
	    ;; Specbind all the special variables.
	    (dolist (spec-var spec-vars)
	      (elcomp--add-call compiler nil :elcomp-specbind
				(list
				 (elcomp--constant "constant"
						   :value (car spec-var))
				 (cdr spec-var))))
	    (elcomp--push-fake-unwind-protect compiler (length spec-vars)))
	  ;; Now evaluate the body of the let.
	  (elcomp--linearize-body compiler (cddr form) result-location)
	  ;; And finally unbind.
	  (when spec-vars
	    (elcomp--pop-fake-unwind-protects compiler (length spec-vars))
	    (elcomp--add-call compiler nil :elcomp-unbind
			      (list
			       (elcomp--constant "constant"
						 :value (length spec-vars)))))))

       ((eq fn 'let*)
	;; Arrange to reset the rewriting table outside the 'let*'.
	(cl-letf (((elcomp--rewrite-alist compiler)
		   (elcomp--rewrite-alist compiler))
		  (num-specbinds 0))
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
	      ;; Make it visible to subsequent blocks.
	      (if (special-variable-p sym)
		  (progn
		    (elcomp--add-call compiler nil :elcomp-specbind
				      (list
				       (elcomp--constant "constant" :value sym)
				       sym-result))
		    (elcomp--push-fake-unwind-protect compiler 1)
		    (cl-incf num-specbinds))
		(push (cons sym sym-result) (elcomp--rewrite-alist compiler)))))
	  ;; Evaluate the body of the let*.
	  (elcomp--linearize-body compiler (cddr form) result-location)
	  ;; And finally unbind.
	  (when (> num-specbinds 0)
	    (elcomp--pop-fake-unwind-protects compiler num-specbinds)
	    (elcomp--add-call compiler nil :elcomp-unbind
			      (list
			       (elcomp--constant "constant"
						 :value num-specbinds))))))

       ((eq fn 'setq-default)
	(setf form (cdr form))
	(while form
	  (let* ((sym (pop form))
		 (val (pop form))
		 ;; We store the last result but drop the others.
		 (stored-variable (if form nil result-location))
		 (intermediate (elcomp--new-var compiler)))
	    ;; This is translated straightforwardly as a call to
	    ;; `set-default'.
	    (elcomp--linearize compiler val intermediate)
	    (elcomp--add-call compiler stored-variable
			      'set-default
			      (list (elcomp--constant "constant" :value sym)
				    intermediate)))))

       ((eq fn 'setq)
	(setf form (cdr form))
	(while form
	  (let* ((sym (pop form))
		 (val (pop form))
		 ;; We store the last `setq' but drop the results of
		 ;; the rest.
		 (stored-variable (if form nil result-location)))
	    (if (special-variable-p sym)
		(let ((intermediate (elcomp--new-var compiler)))
		  ;; A setq of a special variable is turned into a
		  ;; call to `set'.  Our "set" instruction is reserved
		  ;; for ordinary variables.
		  (elcomp--linearize compiler val intermediate)
		  (elcomp--add-call compiler stored-variable
				    'set
				    (list (elcomp--constant "constant"
							    :value sym)
					  intermediate)))
	      ;; An ordinary `setq' is turned into a "set"
	      ;; instruction.
	      (let ((rewritten-sym (elcomp--rewrite-one-ref compiler sym)))
		(elcomp--linearize compiler val rewritten-sym)
		(when stored-variable
		  ;; Return the value.
		  (elcomp--add-set compiler result-location stored-variable)))))))

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
	      (elcomp--add-if compiler this-cond-var nil next-label)
	      ;; The body.
	      (if (cdr clause)
		  (elcomp--linearize-body compiler
					  (cdr clause) result-location))
	      ;; Done.  Cleaning up unnecessary labels happens in
	      ;; another pass, so we can be a bit lazy here.
	      (elcomp--add-goto compiler label-done)
	      (elcomp--make-block-current compiler next-label)))
	  (elcomp--make-block-current compiler label-done)))

       ((memq fn '(progn inline))
	(elcomp--linearize-body compiler (cdr form) result-location))
       ((eq fn 'prog1)
	(elcomp--linearize-body compiler (cdr form) result-location 1))
       ((eq fn 'prog2)
	(elcomp--linearize-body compiler (cdr form) result-location 2))

       ((eq fn 'while)
	(let ((label-top (elcomp--label compiler))
	      (label-done (elcomp--label compiler))
	      (cond-var (elcomp--new-var compiler)))
	  (if result-location
	      (elcomp--add-set compiler result-location
			       (elcomp--operand compiler nil)))
	  ;; FIXME: set the result-location
	  (elcomp--make-block-current compiler label-top)
	  ;; The condition expression and goto.
	  (elcomp--linearize compiler (cadr form) cond-var)
	  (elcomp--add-if compiler cond-var nil label-done)
	  ;; The body.
	  (elcomp--linearize-body compiler (cddr form) nil)
	  (elcomp--add-goto compiler label-top)
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'if)
	(let ((label-false (elcomp--label compiler))
	      (label-done (elcomp--label compiler))
	      (cond-var (elcomp--new-var compiler)))
	  ;; The condition expression and goto.
	  (elcomp--linearize compiler (cadr form) cond-var)
	  (elcomp--add-if compiler cond-var nil label-false)
	  ;; The true branch.
	  (elcomp--linearize compiler (cl-caddr form) result-location)
	  ;; The end of the true branch.
	  (elcomp--add-goto compiler label-done)
	  ;; The false branch.
	  (elcomp--make-block-current compiler label-false)
	  (if (cl-cdddr form)
	      (elcomp--linearize-body compiler (cl-cdddr form) result-location))
	  ;; The end of the statement.
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'and)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (condition (cdr form))
	    (let ((result-location (or result-location
				       (elcomp--new-var compiler))))
	      (elcomp--linearize compiler condition result-location)
	      ;; We don't need this "if" for the last iteration, and
	      ;; "and" in conditionals could be handled better -- but
	      ;; all this is fixed up by the optimizers.
	      (elcomp--add-if compiler result-location nil label-done)))
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'or)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (condition (cdr form))
	    (let ((result-location (or result-location
				       (elcomp--new-var compiler))))
	      (elcomp--linearize compiler condition result-location)
	      (elcomp--add-if compiler result-location label-done nil)))
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'catch)
	(let* ((tag (elcomp--operand compiler (cadr form)))
	       (handler-label (elcomp--label compiler))
	       (done-label (elcomp--label compiler))
	       (exception (elcomp--catch "catch"
					 :handler handler-label
					 :tag tag)))
	  (push exception (elcomp--exceptions compiler))
	  ;; We need a new block because we have modified the
	  ;; exception handler list.
	  (elcomp--make-block-current compiler (elcomp--label compiler))
	  (elcomp--linearize-body compiler (cddr form) result-location)
	  ;; The catch doesn't cover the handler; but pop before the
	  ;; "goto" so the new block has the correct exception list.
	  (pop (elcomp--exceptions compiler))
	  (elcomp--add-goto compiler done-label)
	  (elcomp--make-block-current compiler handler-label)
	  ;; A magic call to get the value.
	  (elcomp--add-call compiler result-location
			    :catch-value nil)
	  (elcomp--add-goto compiler done-label)
	  (elcomp--make-block-current compiler done-label)))

       ((eq fn 'unwind-protect)
	(let ((handler-label (elcomp--label compiler))
	      (done-label (elcomp--label compiler))
	      (normal-label (elcomp--label compiler)))
	  (push (elcomp--unwind-protect "unwind-protect"
					:handler handler-label
					:original-form (cons 'progn
							     (cddr form)))
		(elcomp--exceptions compiler))
	  ;; We need a new block because we have modified the
	  ;; exception handler list.
	  (elcomp--make-block-current compiler (elcomp--label compiler))
	  (elcomp--linearize compiler (cadr form) result-location)
	  ;; The catch doesn't cover the handler; but pop before the
	  ;; "goto" so the new block has the correct exception list.
	  (pop (elcomp--exceptions compiler))
	  (elcomp--add-goto compiler normal-label)
	  (elcomp--make-block-current compiler normal-label)
	  ;; We double-linearize the handlers because this is simpler
	  ;; and usually better.
	  (elcomp--linearize-body compiler (cddr form)
				  (elcomp--new-var compiler))
	  (elcomp--add-goto compiler done-label)
	  (elcomp--make-block-current compiler handler-label)
	  ;; The second linearization.
	  (elcomp--linearize-body compiler (cddr form)
				  (elcomp--new-var compiler))
	  (elcomp--add-call compiler nil :unwind-protect-continue nil)
	  (elcomp--make-block-current compiler done-label)))

       ((eq fn 'condition-case)
	(error "somehow a condition-case made it through macro expansion"))

       ((eq fn :elcomp-condition-case)
	(let ((new-exceptions nil)
	      (body-label (elcomp--label compiler))
	      (done-label (elcomp--label compiler))
	      (saved-exceptions (elcomp--exceptions compiler)))
	  ;; We emit the handlers first because it is a bit simpler
	  ;; here, and it doesn't matter for the result.
	  (elcomp--add-goto compiler body-label)
	  (dolist (handler (cddr form))
	    (let ((this-label (elcomp--label compiler)))
	      (push (elcomp--condition-case "condition-case"
					    :handler this-label
					    :condition-name (car handler))
		    new-exceptions)
	      (elcomp--make-block-current compiler this-label)
	      ;; Note that here we probably pretend that the handler
	      ;; block is surrounded by '(let ((var ...))...)'.  This
	      ;; is done by a compiler macro, which explains why
	      ;; there's no special handling here.
	      (elcomp--linearize-body compiler (cdr handler) result-location)
	      (elcomp--add-goto compiler done-label)))
	  ;; Careful with the ordering.
	  (setf new-exceptions (nreverse new-exceptions))
	  (dolist (exception new-exceptions)
	    (push exception (elcomp--exceptions compiler)))
	  ;; Update the body label's list of exceptions.
	  (setf (elcomp--basic-block-exceptions body-label)
		(elcomp--exceptions compiler))
	  (elcomp--make-block-current compiler body-label)
	  (elcomp--linearize compiler (cadr form) result-location)
	  ;; The catch doesn't cover the handler; but pop before the
	  ;; "goto" so the new block has the correct exception list.
	  (setf (elcomp--exceptions compiler) saved-exceptions)
	  (elcomp--add-goto compiler done-label)
	  (elcomp--make-block-current compiler done-label)))

       ((eq fn 'interactive)
	nil)

       ((not (symbolp fn))
	;; FIXME - lambda or the like
	(error "not supported: %S" fn)
	)

       ((special-form-p (symbol-function fn))
	(error "unhandled special form: %s" (symbol-name fn)))

       (t
	;; An ordinary function call.
	(let ((these-args
	       ;; Compute each argument.
	       (mapcar (lambda (arg) (elcomp--operand compiler arg))
		       (cdr form))))
	  ;; Make the call.
	  (elcomp--add-call compiler result-location fn these-args)))))))

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
	      (nconc (elcomp--defun compiler) (list (cadar form))))
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

(defun elcomp--translate (form)
  (byte-compile-close-variables
   (let* ((byte-compile-macro-environment
	   (cons '(condition-case . elcomp--condition-case)
		 (cons '(declare . elcomp--declare)
		       byte-compile-macro-environment)))
	  (compiler (make-elcomp))
	  (result-var (elcomp--new-var compiler))
	  (code nil))
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

;;; linearize.el ends here
