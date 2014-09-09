;;; Dominators.

(defun elcomp--reverse-postorder (compiler)
  "Return a list of basic blocks from COMPILER, in reverse postorder."
  (let ((result))
    (elcomp--iterate-over-bbs compiler (lambda (bb)
					 (push bb result))
			      t)))

(defun elcomp--first-processed-predecessor (bb doms)
  fixme)

(defun elcomp--intersect (bb1 bb2 doms postorder-number)
  (let ((f1 (gethash postorder-number bb1))
	(f2 (gethash postorder-number bb2)))
    (while (not (eq f1 f2))
      (while (< f1 f2)
	(setf bb1 (gethash bb1 doms))
	(setf f1 (gethash postorder-number bb1)))
      (while (< f2 f1)
	(setf bb2 (gethash bb2 doms))
	(setf f2 (gethash postorder-number bb2))))
    bb1))

(defun elcomp--compute-dominators (compiler)
  (let ((doms (make-hash-table))
	(reversed (elcomp--reverse-postorder compiler))
	(changed t)
	(postorder-number (make-hash-table)))

    ;; Perhaps DOMS and POSTORDER-NUMBER should simply be attributes
    ;; on the BBs.
    (let ((i 0))
      (dolist (bb reversed)
	(puthash bb i postorder-number)
	(cl-incf i)))

    (setf postorder-number (delq (elcomp--entry-block compiler)
				 postorder-number))
    (puthash (elcomp--entry-block compiler) (elcomp--entry-block compiler)
	     doms)

    (while changed
      (setf changed nil)
      (dolist (bb reversed)
	(let ((new-idom (elcomp--first-processed-predecessor bb doms)))
	  (dolist (pred (elcomp--predecessors bb))
	    (unless (eq new-idom pred)
	      (if (gethash pred doms)
		  (setf new-idom (elcomp--intersect pred new-idom)))))
	  (unless (eq new-idom (gethash bb doms))
	    (puthash bb new-idom doms)
	    (setf changed t)))))))
