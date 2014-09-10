(defun elcomp-declare (func &rest props)
  "Apply PROPS, a plist of attributes, to FUNC, a symbol.

Defined properties are:

  :elcomp-const t|nil        If t, FUNC only examines its arguments, not memory.
  :elcomp-type TYPE          The return type of FUNC.
  :elcomp-simple-numeric t|n If t, FUNC ..."
  ;; add more?
  ;; :pure - like const but can refer to memory - e.g., car
  ;; :nothrow - can't signal or throw
  ;; :malloc - allocates new object
  ;; :primitive - assume this can never be rewritten, e.g. car
  ;; ... though if a function has any properties then we're already
  ;; assuming that.
  (while props
    (put func (car props) (cadr props))
    (setf props (cddr props))))

(defun elcomp--const-p (func)
  (get func :elcomp-const))

(defun elcomp--type (func)
  (get func :elcomp-type))

(defun elcomp--simple-numeric-p (func)
  (get func :elcomp-simple-numeric))

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

(dolist (func '(lsh ash logand logior logxor lognot byteorder))
  (elcomp-declare func :elcomp-const t :elcomp-type 'integer))

(elcomp-declare 'cons :elcomp-type 'list)
