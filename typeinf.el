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
  (let ((found-type (get var :type)))
    (if found-type
	(if (not (eq found-type type-name))
	    (error "variable already has a type"))))
  (put var :type type-name))

;; this was in the old elcomp--append,
;; which added to the instruction list

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
		(elcomp--set-type result-location found-type))))))

;; this was in elcomp--linearize
       ((eq fn 'declare)
	(dolist (spec (cdr form))
	  ;; FIXME this should also examine direct-calls
	  (pcase spec
	      (`(type ,type-name . ,variables)
	       (dolist (var variables)
		 (setf var (elcomp--rewrite-one-ref compiler var))
		 (elcomp--set-type var type-name))))))

;; was used by c translation
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

(defconst elcomp--simple-math
  '(< <= > >= /= + - * / 1+))

;; used by eltoc
(defun elcomp--check-simple-math (form)
  (and (eq (car form) 'call)
       (memq (nth 2 form) elcomp--simple-math)
       (let ((found-type (elcomp--any-argument-typed-p (nthcdr 3 form))))
	 (if found-type
	     (dolist (arg (nthcdr 3 form))
	       (if (symbolp arg)
		   (elcomp--set-type arg found-type)))))))

