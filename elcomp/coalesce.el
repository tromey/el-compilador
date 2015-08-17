;;; coalesce.el --- Coalesce blocks. -*- lexical-binding:t -*-

;;; Commentary:

;; A simple pass to coalesce blocks when possible.

;;; Code:

(require 'elcomp)
(require 'elcomp/back)

(defun elcomp--coalesce-pass (compiler)
  "A compiler pass to coalesce blocks.

A block can be coalesced with a second block if the second block
is the sole successor of the original, and the original is the
sole predecessor of the second, and if they have compatible
outgoing exception edges."
  (elcomp--require-back-edges compiler)
  (let ((rewrote-one nil)
	(ever-rewrote-one nil))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       ;; Loop until we're done with this block.
       (setf rewrote-one t)
       (while rewrote-one
	 (setf rewrote-one nil)
	 (when ;; If there is just one successor...
	     (elcomp--goto-p (elcomp--last-instruction bb))
	   (let ((succ
		  (oref (elcomp--last-instruction bb) :block)))
	     (when (and
		    ;; and the successor block has a single predecessor...
		    (= (hash-table-count (elcomp--basic-block-parents succ)) 1)
		    ;; and either...
		    (or
		     ;; the exception regions are the same -- we can
		     ;; use `eq' due to how the exception lists are
		     ;; constructed...
		     (eq (elcomp--basic-block-exceptions bb)
			 (elcomp--basic-block-exceptions succ))
		     ;; or this block is empty, in which case its
		     ;; exception regions are immaterial...
		     (eq (elcomp--basic-block-code bb)
			 (elcomp--basic-block-code-link bb))))
	       ;; ... we can coalesce the blocks.
	       (setf (elcomp--basic-block-code bb)
		     (append
		      (nbutlast (elcomp--basic-block-code bb))
		      (elcomp--basic-block-code succ)))
	       (setf (elcomp--basic-block-code-link bb)
		     (elcomp--basic-block-code-link succ))
	       ;; If the current block was empty, then we need to take
	       ;; the exceptions from the successor block.  It doesn't
	       ;; hurt to do this unconditionally.
	       (setf (elcomp--basic-block-exceptions bb)
		     (elcomp--basic-block-exceptions succ))
	       (setf rewrote-one t)
	       (setf ever-rewrote-one t)))))))
    (when ever-rewrote-one
      (elcomp--invalidate-cfg compiler))))

(provide 'elcomp/coalesce)

;;; coalesce.el ends here
