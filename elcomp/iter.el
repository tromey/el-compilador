;;; iter.el --- iterate over blocks.  -*- lexical-binding:t -*-

;;; Code:

(require 'elcomp)

(defun elcomp--do-iterate (hash callback bb postorder)
  (unless (gethash bb hash)
    (puthash bb t hash)
    (unless postorder
      (funcall callback bb))
    (let ((obj (elcomp--last-instruction bb)))
      (cond
       ;; FIXME why is the -child- variant needed here?
       ((elcomp--goto-p obj)
	(elcomp--do-iterate hash callback (elcomp--block obj) postorder))
       ((elcomp--if-p obj)
	(elcomp--do-iterate hash callback (elcomp--block-true obj) postorder)
	(elcomp--do-iterate hash callback (elcomp--block-false obj) postorder))))
    (dolist (exception (elcomp--basic-block-exceptions bb))
      (when (oref exception :handler)
	(elcomp--do-iterate hash callback (oref exception :handler) postorder)))
    (when postorder
      (funcall callback bb))))

(defun elcomp--iterate-over-bbs (compiler callback &optional postorder)
  (elcomp--do-iterate (make-hash-table) callback
		      (elcomp--entry-block compiler)
		      postorder))

(defun elcomp--postorder (compiler)
  "Return a list of basic blocks from COMPILER, in postorder."
  (let ((result))
    (elcomp--iterate-over-bbs compiler (lambda (bb)
					 (push bb result))
			      t)
    (nreverse result)))

(defun elcomp--reverse-postorder (compiler)
  "Return a list of basic blocks from COMPILER, in reverse postorder."
  (nreverse (elcomp--postorder compiler)))

(provide 'elcomp/iter)

;;; iter.el ends here
