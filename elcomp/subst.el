;;; subst.el --- simple substitutions. -*- lexical-binding:t -*-

;;; Commentary:

;; This is some utility code to rewrite SSA names in a compiler
;; instance.  The caller provides a map and all the instructions are
;; updated according to the map.

;;; Code:

(require 'elcomp)

(cl-defgeneric elcomp--rewrite-insn (insn map)
  "Rewrite INSN according to MAP.

MAP is a hash table mapping old instructions to new ones.")

(cl-defmethod elcomp--rewrite-insn (insn _map)
  "Unhandled cases call `error'."
  (error "unhandled case: %S" insn))

(cl-defmethod elcomp--rewrite-insn ((insn elcomp--set) map)
  (let ((new-insn (gethash (elcomp--value insn) map)))
    (when new-insn
      (setf (elcomp--value insn) new-insn))))

(cl-defmethod elcomp--rewrite-insn ((insn elcomp--call) map)
  ;; FIXME: the :func slot?
  (cl-mapl
   (lambda (cell)
     (let ((new-insn (gethash (car cell) map)))
       (when new-insn
	 (setf (car cell) new-insn))))
   (elcomp--args insn)))

(cl-defmethod elcomp--rewrite-insn ((_insn elcomp--goto) _map)
  nil)

(cl-defmethod elcomp--rewrite-insn ((insn elcomp--if) map)
  (let ((new-insn (gethash (elcomp--sym insn) map)))
    (when new-insn
      (setf (elcomp--sym insn) new-insn))))

(cl-defmethod elcomp--rewrite-insn ((insn elcomp--return) map)
  (let ((new-insn (gethash (elcomp--sym insn) map)))
    (when new-insn
      (setf (elcomp--sym insn) new-insn))))

(cl-defmethod elcomp--rewrite-insn ((insn elcomp--phi) map)
  ;; Ugh.
  (let ((new-hash (make-hash-table)))
    (maphash
     (lambda (phi _ignore)
       (let ((subst (gethash phi map)))
	 (puthash
	  ;; It never makes sense to propagate a constant into a phi.
	  (if (elcomp--constant-p subst)
	      phi
	    (or subst phi))
	  t new-hash)))
     (elcomp--args insn))
    (setf (elcomp--args insn) new-hash)))

;; FIXME `elcomp--catch's :tag?

(defun elcomp--rewrite-using-map (compiler map)
  "Rewrite all the instructions in COMPILER according to MAP.

MAP is a hash table that maps old operands to new ones."
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (maphash (lambda (_ignore phi)
		(elcomp--rewrite-insn phi map))
	      (elcomp--basic-block-phis bb))
     (dolist (insn (elcomp--basic-block-code bb))
       (elcomp--rewrite-insn insn map)))))

(provide 'elcomp/subst)

;;; subst.el ends here
