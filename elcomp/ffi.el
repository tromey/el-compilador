
;; FIXME this should probably instead find a way to compile LIBRARY to
;; a function-scoped static.
(defun elcomp--define-ffi-library (symbol name)
  (let ((library (cl-gensym)))
    `(defun ,symbol ()
       ;; FIXME this is lame but until we handle defvar properly...
       (unless (boundp ',library)
	 ;; FIXME this really ought to be some low-level type anyhow.
	 (setq ,library (ffi--dlopen ,name))))))

(defconst elcomp--ffi-type-map
  '((:int8 . integer)
    (:uint8 . integer)
    (:int16 . integer)
    (:uint16 . integer)
    (:int32 . integer)
    (:uint32 . integer)
    (:int64 . integer)
    (:uint64 . integer)
    (:float . float)
    (:double . float)
    (:uchar . integer)
    (:schar . integer)
    (:char . integer)
    (:ushort . integer)
    (:short . integer)
    (:uint . integer)
    (:int . integer)
    (:ulong . integer)
    (:long . integer)
    (:ulonglong . integer)
    (:longlong . integer)
    (:size_t . integer)
    (:ssize_t . integer)
    (:ptrdiff_t . integer)
    (:wchar_t . integer)
    (:bool . boolean)
    ;; :pointer - but it doesn't really matter that one is missing
    ;; here.
    ))

(defun elcomp--define-ffi-function (name c-name return-type arg-types library)
  (let* ((arg-names (mapcar (lambda (_ignore) (cl-gensym)) arg-types))
	 (type-checks (cl-mapcar
		(lambda (arg type)
		  `(cl-check-type ,arg
				  ,(cdr (assq type elcomp--ffi-type-map))))
		arg-names arg-types))
	 (func-pointer (cl-gensym)))
    ;; FIXME this is kind of lying about the return type for :bool
    (elcomp-declare name :elcomp-type
		    (cdr (assq return-type elcomp--ffi-type-map)))
    ;; FIXME if we had a lower-level type system, then we could inline
    ;; this when we have type information and eliminate checks.
    `(defun ,name ,arg-names
       ;; FIXME another lameness until we can handle defvar and make a
       ;; function- or file-scoped static.
       (unless (boundp ',func-pointer)
	 ;; FIXME this really ought to be some low-level type anyhow.
	 (setq ,func-pointer (ffi--dlsym ,c-name (,library))))
       ,@type-checks
       (:ffi-call ,func-pointer ,@arg-names))))

(defun elcomp--use-ffi ()
  (push '(define-ffi-library . elcomp--define-ffi-library) elcomp--compiler-macros)
  (push '(define-ffi-function . elcomp--define-ffi-function) elcomp--compiler-macros))
