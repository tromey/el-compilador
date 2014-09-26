;; TO DO:

;; Ideas:
;; * always inline mapcar &c

;; * Why doesn't mapconcat let the lambda return chars?

;; Notes
;; * I wonder if progv is implemented correctly now that
;;   macroexpand is done eagerly

(require 'cl-macs)
(require 'eieio)

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
  ;; The entry block.
  entry-block
  ;; The current basic block.
  current-block
  ;; True if the back-edges in the CFG are considered valid.
  ;; FIXME - deal with IDOM being invalid too
  back-edges-valid
  ;; The current list of exception handlers.
  exceptions
  ;; The current defun being compiled.
  ;; This is a list (NAME ARGLIST DOC INTERACTIVE).
  defun
  ;; A list of all defun symbols being compiled.
  defuns)

(cl-defstruct elcomp--basic-block
  ;; Block number.
  number
  ;; The code for this basic block.
  code
  ;; Last link of linearized code.
  code-link
  ;; A hash table holding back-links to parent nodes.
  ;; Outgoing edges are represented directly by the last instruction
  ;; in the code sequence.
  parents
  ;; The immediate dominator, or nil if not known.
  immediate-dominator
  ;; The list of exception handlers.
  exceptions
  )

(defclass elcomp--set nil
  ((sym :initform nil :initarg :sym)
   (value :initform nil :initarg :value)))

(defclass elcomp--call nil
  ((sym :initform nil :initarg :sym)
   (func :initform nil :initarg :func)
   (args :initform nil :initarg :args)))

(defclass elcomp--goto nil
  ((block :initform nil :initarg :block)))

(defclass elcomp--if nil
  ((sym :initform nil :initarg :sym)
   (block-true :initform nil :initarg :block-true)
   (block-false :initform nil :initarg :block-false)))

(defclass elcomp--return nil
  ((sym :initform nil :initarg :sym)))

(defclass elcomp--diediedie (elcomp--call)
  ()
  "An instruction which terminates a basic block without leading anywhere.

This can only be used after a call to a `nothrow' function.")

;; An SSA operand representing a constant.
(defclass elcomp--constant nil
  ((value :initform nil :initarg :value)))

(defclass elcomp--phi nil
  ((sym :initform nil :initarg :sym)
   (args :initform nil :initarg :args)))

;; An exception edge.
(defclass elcomp--exception nil
  ((handler :initform nil :initarg :handler)))

;; A catch.
(defclass elcomp--catch (elcomp--exception)
  ((tag :initform nil :initarg :tag)
   (result :initform nil :initarg :result)))

;; A single condition-case handler.
(defclass elcomp--condcase (elcomp--exception)
  ((variable :initform nil :initarg :variable)
   (condition-name :initform nil :initarg :condition-name)))

;; An unwind-protect.
(defclass elcomp--unwind-protect (elcomp--exception)
  ())

(defun elcomp--ssa-name-p (arg)
  (or
   (elcomp--set-child-p arg)
   (elcomp--phi-child-p arg)))

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
    result))

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
       (not (keywordp obj))))

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
	      (elcomp--add-set compiler result-location last-rewritten-sym))))

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
	      (elcomp--add-set compiler result-location nil))
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
	  (elcomp--linearize compiler (caddr form) result-location)
	  ;; The end of the true branch.
	  (elcomp--add-goto compiler label-done)
	  ;; The false branch.
	  (elcomp--make-block-current compiler label-false)
	  (if (cdddr form)
	      (elcomp--linearize-body compiler (cdddr form) result-location))
	  ;; The end of the statement.
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'and)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (condition (cdr form))
	    (elcomp--linearize compiler condition result-location)
	    ;; We don't need this "if" for the last iteration, and
	    ;; "and" in conditionals could be handled better -- but
	    ;; all this is fixed up by the optimizers.
	    (elcomp--add-if compiler result-location label-done nil))
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'or)
	(let ((label-done (elcomp--label compiler)))
	  (dolist (condition (cdr form))
	    (elcomp--linearize compiler condition result-location)
	    (elcomp--add-if compiler result-location label-done nil))
	  (elcomp--make-block-current compiler label-done)))

       ((eq fn 'catch)
	(let* ((tag (elcomp--operand compiler (cadr form)))
	       (handler-label (elcomp--label compiler))
	       (done-label (elcomp--label compiler))
	       (exception (elcomp--catch "catch"
					 :handler handler-label
					 :tag tag
					 :result result-location)))
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
	  ;; This block magically sets RESULT-LOCATION... ?
	  ;; Or we could emit a special internal call to fetch
	  ;; the data.  FIXME.
	  (elcomp--add-goto compiler done-label)
	  (elcomp--make-block-current compiler done-label)))

       ((eq fn 'unwind-protect)
	(let ((handler-label (elcomp--label compiler))
	      (done-label (elcomp--label compiler))
	      (normal-label (elcomp--label compiler)))
	  (push (elcomp--unwind-protect "unwind-protect"
					:handler handler-label)
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
	(let ((new-exceptions nil)
	      (body-label (elcomp--label compiler))
	      (done-label (elcomp--label compiler))
	      (saved-exceptions (elcomp--exceptions compiler)))
	  ;; We emit the handlers first because it is a bit simpler
	  ;; here, and it doesn't matter for the result.
	  (elcomp--add-goto compiler body-label)
	  (dolist (handler (cdddr form))
	    (let ((this-label (elcomp--label compiler)))
	      (push (elcomp--condcase "condition-case"
				      :handler this-label
				      :variable (cadr form)
				      :condition-name (car handler))
		    new-exceptions)
	      (elcomp--make-block-current compiler this-label)
	      ;; Here we should probably pretend that the
	      ;; handler block is surrounded by '(let ((var ...))...)'.
	      ;; VAR might be defvar'd.
	      ;; Maybe we could emit a special call like
	      ;; (setq VAR (:internal-function:))
	      ;; to fetch the data.
	      (elcomp--linearize-body compiler (cdr handler) result-location)
	      (elcomp--add-goto compiler done-label)))
	  ;; Careful with the ordering.
	  (setf new-exceptions (nreverse new-exceptions))
	  (dolist (exception new-exceptions)
	    (push exception (elcomp--exceptions compiler)))
	  ;; Update the body label's list of exceptions.
	  (oset (elcomp--basic-block-exceptions body-label)
		:exceptions (elcomp--exceptions compiler))
	  (elcomp--make-block-current compiler body-label)
	  (elcomp--linearize compiler (caddr form) result-location)
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

(defun elcomp--optimize (compiler)
  (elcomp--thread-jumps-pass compiler)
  (elcomp--eh-cleanup-pass compiler)
  (elcomp--coalesce-pass compiler)
  (elcomp--require-back-edges compiler)
  (elcomp--compute-dominators compiler))

(defun elcomp--translate (form)
  (byte-compile-close-variables
   (let* ((byte-compile-macro-environment
	   (cons '(declare . elcomp--declare)
		 byte-compile-macro-environment))
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



(defun elcomp--do-iterate (hash callback bb postorder)
  (unless (gethash bb hash)
    (puthash bb t hash)
    (unless postorder
      (funcall callback bb))
    (let ((obj (elcomp--last-instruction bb)))
      (cond
       ;; FIXME why is the -child- variant needed here?
       ((elcomp--goto-child-p obj)
	(elcomp--do-iterate hash callback (oref obj :block) postorder))
       ((elcomp--if-child-p obj)
	(elcomp--do-iterate hash callback (oref obj :block-true) postorder)
	(elcomp--do-iterate hash callback (oref obj :block-false) postorder))))
    (dolist (exception (elcomp--basic-block-exceptions bb))
      (elcomp--do-iterate hash callback (oref exception :handler) postorder))
    (when postorder
      (funcall callback bb))))

(defun elcomp--iterate-over-bbs (compiler callback &optional postorder)
  (elcomp--do-iterate (make-hash-table) callback
		      (elcomp--entry-block compiler)
		      postorder))
