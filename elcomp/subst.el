;;; subst.el --- simple substitutions. -*- lexical-binding:t -*-

;;; Commentary:

;; This is some utility code to rewrite SSA names in a compiler
;; instance.  The caller provides a map and all the instructions are
;; updated according to the map.

;;; Code:

(require 'elcomp)

(defgeneric elcomp--rewrite-insn (insn map)
  "Rewrite INSN according to MAP.

MAP is a hash table mapping old instructions to new ones.")

(defmethod elcomp--rewrite-insn (insn map)
  "Unhandled cases call `error'q."
  (error "unhandled case: %S" insn))

(defmethod elcomp--rewrite-insn ((insn elcomp--set) map)
  (let ((new-insn (gethash (oref insn :value) map)))
    (when new-insn
      (setf (oref insn :value) new-insn))))

(defmethod elcomp--rewrite-insn ((insn elcomp--call) map)
  ;; FIXME: the :func slot?
  (cl-mapl
   (lambda (cell)
     (let ((new-insn (gethash (car cell) map)))
       (when new-insn
	 (setf (car cell) new-insn))))
   (oref insn :args)))

(defmethod elcomp--rewrite-insn ((insn elcomp--goto) map)
  nil)

(defmethod elcomp--rewrite-insn ((insn elcomp--if) map)
  (let ((new-insn (gethash (oref insn :sym) map)))
    (when new-insn
      (setf (oref insn :sym) new-insn))))

(defmethod elcomp--rewrite-insn ((insn elcomp--return) map)
  (let ((new-insn (gethash (oref insn :sym) map)))
    (when new-insn
      (setf (oref insn :sym) new-insn))))

(defmethod elcomp--rewrite-insn ((insn elcomp--phi) map)
  ;; Ugh.
  (let ((new-hash (make-hash-table)))
    (maphash
     (lambda (phi _ignore)
       (puthash (or (gethash phi map) phi) t new-hash))
     (oref insn :args))
    (setf (oref insn :args) new-hash)))

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
