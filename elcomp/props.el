;;; props.el --- Function properties. -*- lexical-binding:t -*-

;;; Commentary:

;; This module has code to note properties of functions.  The
;; properties in question are those which are of interest to the
;; compiler, and which are considered immutable -- currently it is
;; only possible for the compiler to handle properties of functions
;; that the user cannot reasonably redefine.

;; byte-compile-negated-op is not quite useful
;; but the idea could be applied
;; normalizing is valuable for generic optimizations

;;; Code:

(require 'elcomp)

(defun elcomp-declare (func &rest props)
  "Apply PROPS, a plist of attributes, to FUNC, a symbol.

Defined properties are:

  :elcomp-const t|nil        If t, FUNC does not have side effects.
                             This means a call to it can be removed if
                             its return value is not used.
  :elcomp-pure t|nil         Like :elcomp-const, but also does not
                             refer to memory.
  :elcomp-type TYPE          The return type of FUNC.
  :elcomp-simple-numeric t|n If t, FUNC is a simple numeric function.  This
                             means that it accepts a number of
                             integer, marker, or float arguments,
                             and that the type of the result
                             follows the usual contagion rules.  Such a
                             function can never return `nil'.
  :elcomp-predicate TYPE     This function is a type predicate that
                             tests for TYPE.
  :elcomp-noreturn t|nil     If t, FUNC does not return normally.
  :elcomp-nothrow t|nil      If t, FUNC cannot `throw' or `signal'.
  :elcomp-direct t|nil       If t, generated C code can call this directly."
  ;; add more?
  ;; :malloc - allocates new object
  ;; :primitive - assume this can never be rewritten, e.g. car
  ;; ... though if a function has any properties then we're already
  ;; assuming that.
  ;; :commutative - then we could sort arguments somehow and
  ;;         have more CSE opportunities
  (while props
    (put func (car props) (cadr props))
    (setf props (cddr props))))

(defun elcomp--func-const-p (func)
  "Return t if FUNC can be considered 'const'."
  (or (get func :elcomp-const)
      (get func 'side-effect-free)))

(defun elcomp--func-pure-p (func)
  "Return t if FUNC can be considered 'pure'."
  (or (get func :elcomp-pure)
      (get func 'pure)))

(defun elcomp--func-type (func)
  "Return the type of FUNC, if known, or nil."
  (get func :elcomp-type))

(defun elcomp--func-simple-numeric-p (func)
  "Return t if FUNC can be considered 'simple-numeric'."
  (get func :elcomp-simple-numeric))

(defun elcomp--func-type-predicate (func)
  "If FUNC is a type predicate, return the corresponding type, else nil."
  (get func :elcomp-predicate))

(defun elcomp--func-noreturn-p (func)
  "Return t if FUNC can be considered 'noreturn'."
  (get func :elcomp-noreturn))

(defun elcomp--func-nothrow-p (func)
  "Return t if FUNC can be considered 'nothrow'."
  (or (get func :elcomp-nothrow)
      (eq (get func 'side-effect-free) 'error-free)))

(defun elcomp--func-direct-p (func)
  "Return t if FUNC is `direct'-capable from C code.

This is used to limit how many direct calls are emitted.
Indirect calls are generally preferable for `non-trivial'
things, so that advice continues to work."
  (and (symbolp func)
       (get func :elcomp-direct)
       (subrp (symbol-function func))))

(dolist (func '(+ - * / % 1+ 1- mod max min abs expt))
  (elcomp-declare func :elcomp-const t :elcomp-simple-numeric t))

(dolist (func '(isnan floatp integerp numberp natnump zerop = eql eq equal
		      /= < <= > >=))
  (elcomp-declare func :elcomp-const t :elcomp-type 'boolean))

(dolist (func '(ldexp copysign logb float truncate floor ceiling round
		      ffloor fceiling ftruncate fround
		      sin cos tan asin acos atan exp log
		      sqrt))
  (elcomp-declare func :elcomp-const t :elcomp-type 'float))

(dolist (func '(lsh ash logand logior logxor lognot byteorder sxhash))
  (elcomp-declare func :elcomp-const t :elcomp-type 'integer))

(elcomp-declare 'cons :elcomp-type 'cons)
(elcomp-declare 'list :elcomp-type 'list)
(elcomp-declare 'vector :elcomp-type 'vector)
(elcomp-declare 'make-vector :elcomp-type 'vector)
(elcomp-declare 'string :elcomp-type 'string)
(elcomp-declare 'make-string :elcomp-type 'string)
(elcomp-declare 'make-hash-table :elcomp-type 'hash-table)
(elcomp-declare 'intern :elcomp-type 'symbol)
(elcomp-declare 'make-symbol :elcomp-type 'symbol)

;; There are a few type predicates not on the list.  They could be
;; added if needed.  See (elisp) Type Predicates.
(dolist (iter '((atom . list)
		(arrayp . array)
		(bool-vector-p . bool-vector)
		(booleanp . boolean)
		(bufferp . buffer)
		(characterp . integer)	; not clear if this is best
		(consp . cons)
		(floatp . float)
		(hash-table-p . hash-table)
		(integerp . integer)
		(listp . list)
		(markerp . marker)
		(sequencep . sequence)
		(stringp . string)
		(symbolp . symbol)
		(vectorp . vector)
		(wholenump . integer)))
  (elcomp-declare (car iter) :elcomp-predicate (cdr iter)))

(dolist (iter '(throw signal error user-error :unwind-protect-continue))
  (elcomp-declare iter :elcomp-noreturn t))

(dolist (iter '(car-safe cdr-safe sxhash))
  (elcomp-declare iter :elcomp-nothrow t))

(elcomp-declare :elcomp-fetch-condition :elcomp-const t)

;; This first part of this list comes from the bytecode interpreter.
;; Then there are some useful additions.  It's important not to add
;; things here which the user might want to advise.
(dolist (iter '(nth symbolp consp stringp listp eq memq not car cdr
		    cons list length aref aset symbol-value
		    symbol-function set fset get substring concat
		    1- 1+ = > < <= >= - + max min * point
		    goto-char insert point-max point-min char-after
		    following-char preceding-char current-column
		    indent-to eolp eobp bolp bobp current-buffer
		    set-buffer interactive-p forward-char forward-word
		    skip-chars-forward skip-chars-backward forward-line
		    char-syntax buffer-substring delete-region
		    narrow-to-region widen end-of-line
		    set-marker match-beginning match-end upcase
		    downcase string= string< equal nthcdr elt
		    member assq nreverse setcar setcdr car-safe cdr-safe
		    nconc / % numberp integerp
		    ;; These aren't from bytecode.c.
		    funcall apply sxhash))
  (elcomp-declare iter :elcomp-direct t))

(provide 'elcomp/props)

;;; props.el ends here
