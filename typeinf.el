;;; Type inference code. -*- lexical-binding:t -*-

;;; Commentary:

;; The type inference pass attempts to assign types to SSA names.

;; A type is just a symbol as would be returned by 'type-of'.
;; However, there is are some minor differences.  First, (type-of nil)
;; and (type-of t) yield 'symbol, but we represent them as 'null and
;; t.  It's nice to treat these specially as it enables some
;; optimizations.  We also recognize a 'boolean type, which
;; corresponds to the booleanp predicate.  Finally, it's important to
;; know that an object can be of type 'list but have the value nil.
;; That is, one cannot conclude that an object of type 'list is true.
;; This is obviously also true for 'boolean.
;; FIXME - what about 'symbol?  the same applies for symbolp.
;; and (consp nil) is nil, though (listp nil) is t.
;; so some confusion in the model here.

;; Types can be inferred in a few ways:

;; 1. A constant's type is immediately known.
;; 2. Some functions are annotated as returning a known type.
;; 3. Some functions are annotated as being 'special-numeric' functions,
;;    and have special treatment.  See props.el.
;; 4. Type predicates such as integerp are used to annotate
;;    variables.  For example in:
;;        (if (integerp x) (1+ x))
;;    the type of 'x' in the '1+' expression is known to be 'integer.
;;    (This feature is actually not implemented yet.)
;; 5. Type declarations can be used to annotate variables, e.g.:
;;        (let ((x 0)) (declare (type integer i)) ...)
;;    Note that these are not checked, so for argument checking it
;;    is better to use cl-check-type, as its expansion falls under
;;    case 4 above.

;;; Code:

(defun elcomp--set-type (var type-name)
  "Set the type of VAR, an SSA name, to TYPE-NAME, a type."
  (when type-name
    (let ((found-type (get var :elcomp-type)))
      (if found-type
	  (if (not (eq found-type type-name))
	      (error "variable already has a type"))))
    (put var :elcomp-type type-name)))

(defun elcomp--get-type (var)
  "Return the type of VAR, if known, or nil."
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
  "")

(defmethod elcomp--infer-type (insn)
  nil)

(defmethod elcomp--infer-type ((insn elcomp--set))
  (if (elcomp--ssa-name-p (oref insn :value))
      ;; A symbol argument is just a name.  So take its type.
      (elcomp--set-type (oref insn :sym)
			(elcomp--get-type (oref insn :value)))
    ;; If the argument to SET is not a symbol, then it is a
    ;; constant of some kind.
    (let ((const (oref insn value)))
      (cl-assert (elcomp--constant-child-p const))
      (elcomp--set-type (oref insn :sym)
			(type-of (oref insn const :value))))))

(defmethod elcomp--infer-type ((insn elcomp--call))
  (let ((type (elcomp--func-type (oref insn :func))))
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
       (elcomp--infer-type insn)))))

;; this was in elcomp--linearize
       ;; ((eq fn 'declare)
       ;; 	(dolist (spec (cdr form))
       ;; 	  ;; FIXME this should also examine direct-calls
       ;; 	  (pcase spec
       ;; 	      (`(type ,type-name . ,variables)
       ;; 	       (dolist (var variables)
       ;; 		 (setf var (elcomp--rewrite-one-ref compiler var))
       ;; 		 (elcomp--set-type var type-name))))))
