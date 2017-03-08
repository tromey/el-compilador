;; -*- emacs-lisp -*-

(require 'elcomp)
(require 'elcomp/typeinf)

(defvar elcomp--c-compare-type-lists (make-hash-table))

(defun elcomp--define-c-substitution (name type-list substitution)
  (let ((existing (gethash name elcomp--c-compare-type-lists)))
    (push (cons substitution type-list) existing)
    (puthash name existing elcomp--c-compare-type-lists)))

(defun elcomp--c-compare-type-lists (declared-types arg-types)
  ;; FIXME - for now we require eq but we could do better.
  ;; for example an actual type of 'null is ok for 'list.
  (cl-every (lambda (declared-type arg-type)
	      (eq (elcomp--pretend-eval-type-predicate declared-type arg-type)
		  t))
	    declared-types arg-types))

(defun elcomp--c-opt (call types)
  (let ((call-sym (elcomp--func call)))
    (when (symbolp call-sym)
      (cl-dolist (entry (gethash call-sym elcomp--c-compare-type-lists))
	(when (elcomp--c-compare-type-lists (cdr entry) types)
	  ;; Found a match, so optimize.
	  (cl-return (car entry)))))))

(dolist (entry '((car (cons) "XCAR")
		 (cdr (cons) "XCDR")
		 (setcar (cons :bottom) "XSETCAR")
		 (setcdr (cons :bottom) "XSETCDR")
		 (length (vector) "ASIZE")
		 (length (string) "SCHARS")
		 (length (bool-vector) "bool_vector_size")
		 ;; not a function: (length (char-table) "MAX_CHAR")
		 ;; Also: (length (null) 0)
		 (symbol-name (symbol) "SYMBOL_NAME")))
  (apply #'elcomp--define-c-substitution entry))

;; there's no need for this once we fix cprop
;; (elcomp--define-c-substitution car ((arg null))
;; 			"Qnil")
;; (elcomp--define-c-substitution cdr ((arg null))
;; 			"Qnil")

;; (elcomp--define-c-substitution aref ((v vector) (x integer))
;; 			;; what about bounds?
;; 			;; what about XFASTINT
;; 			`("AREF" v x))

;; (elcomp--define-c-substitution null (arg)
;; 			(concat "NILP (" arg ")"))

;; (dolist (simple '(integerp eq floatp markerp symbolp consp
;; 			   stringp bool-vector-p bufferp
;; 			   char-table-p functionp overlayp
;; 			   processp subrp symbolp windowp))
;;   ;; fixme this is wack
;;   (elcomp--do-define-c-substitution simple FIXME upper-case...))

(provide 'elcomp/c-inl)
