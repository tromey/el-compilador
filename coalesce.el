;;; coalesce.el --- Coalesce blocks. -*- lexical-binding:t -*-

;;; Commentary:

;; A simple pass to coalesce blocks when possible.

;;; Code:

(defun elcomp--coalesce-pass (compiler)
  "A compiler pass to coalesce blocks.

A block can be coalesced with a second block if the second block
is the sole successor of the original, and the original is the
sole predecessor of the second, and if they have compatible
outgoing exception edges."
  (elcomp--require-back-edges compiler)
  (let ((rewrote-one nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       ;; Loop until we're done with this block.
       (setf rewrote-one t)
       (while rewrote-one
	 (setf rewrote-one nil)
	 (when ;; If there is just one successor...
	     (elcomp--goto-child-p (elcomp--last-instruction bb))
	   (let ((succ
		  (oref (elcomp--last-instruction bb) :block)))
	     (when (and
		    ;; and the successor block has a single predecessor...
		    (= (hash-table-count (elcomp--basic-block-parents succ)) 1)
		    ;; and the exception regions are the same -- we can
		    ;; use `eq' due to how the exception lists are
		    ;; constructed...
		    (eq (elcomp--basic-block-exceptions bb)
			(elcomp--basic-block-exceptions succ)))
	       ;; ... we can coalesce the blocks.
	       (setf (elcomp--basic-block-code bb)
		     (append
		      (nbutlast (elcomp--basic-block-code bb))
		      (elcomp--basic-block-code succ)))
	       (setf (elcomp--basic-block-code-link bb)
		     (elcomp--basic-block-code-link succ))
	       (setf rewrote-one t)))))))
    (when rewrote-one
      (elcomp--invalidate-cfg compiler))))

;;; coalesce.el ends here
