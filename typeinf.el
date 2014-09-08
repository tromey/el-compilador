;; bogus type inference code


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

(defun elcomp--set-type (var type-name)
  (when type-name
    (let ((found-type (get var :elcomp-type)))
      (if found-type
	  (if (not (eq found-type type-name))
	      (error "variable already has a type"))))
    (put var :elcomp-type type-name)))

(defun elcomp--get-type (var)
  (get var :elcomp-type))

(defun elcomp--check-simple-numeric (compiler call)
  (when (elcomp-simple-numeric-p (oref call :func))
    (let ((type 'unknown))
      (dolist (arg (oref call :args))
	(let ((arg-type (elcomp--get-type arg)))
	  ;; Float overrides integer
	  (when (and (eq type 'integer)
		     (eq arg-type 'float))
	    (setf type arg-type))
	  ;; Unknown overrides anything.
	  (unless arg-type
	    (setf type arg-type))
	  ;; If TYPE is still unknown, take it from the argument.
	  (when (eq type 'unknown)
	    (setf type arg-type))))
      (unless (memq type '(unknown nil))
	(elcomp--set-type (oref call :sym) type)))))

(defgeneric elcomp--infer-type (insn)
  nil)

(defmethod elcomp--infer-type ((insn elcomp--set))
  (if (symbolp (oref insn :value))
      ;; A symbol argument is just a name.  So take its type.
      (elcomp--set-type (oref insn :sym)
			(elcomp--get-type (oref insn :value)))
    ;; If the argument to SET is not a symbol, then it is a
    ;; constant of some kind.
    ;; FIXME this area is still super broken
    (elcomp--set-type (oref insn :sym)
		      ;; Here's the broken spot.
		      (type-of (oref insn :value)))))

(defmethod elcomp--infer-type ((insn elcomp--call))
  (let ((type (elcomp-type (oref insn :func))))
    ;; If the function has a known type, use it.
    (if type
	(elcomp--set-type (oref insn :sym) type)
      ;; Otherwise, handle a possible "simple numeric" call.
      (elcomp--check-simple-numeric insn))))

(defun elcomp--infer-types (compiler)
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (dolist (insn (elcomp--basic-block-code bb))
       (elcomp--infer-types insn)))))

;; this was in elcomp--linearize
       ;; ((eq fn 'declare)
       ;; 	(dolist (spec (cdr form))
       ;; 	  ;; FIXME this should also examine direct-calls
       ;; 	  (pcase spec
       ;; 	      (`(type ,type-name . ,variables)
       ;; 	       (dolist (var variables)
       ;; 		 (setf var (elcomp--rewrite-one-ref compiler var))
       ;; 		 (elcomp--set-type var type-name))))))
