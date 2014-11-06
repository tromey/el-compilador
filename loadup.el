(defconst elcomp--loadup-dir
  (file-name-directory (or load-file-name
			   ;; In the eval-buffer case.
			   (buffer-file-name))))

(defun elcomp--loadup ()
  (interactive)
  (let ((load-path load-path))
    (push elcomp--loadup-dir load-path)
    (dolist (feature '(elcomp
		       elcomp/back
		       elcomp/cmacros
		       elcomp/coalesce
		       elcomp/comp-debug
		       elcomp/cprop
		       elcomp/dce
		       elcomp/dom
		       elcomp/eh-cleanup
		       elcomp/eltoc
		       elcomp/iter
		       elcomp/jump-thread
		       elcomp/linearize
		       elcomp/phiopt
		       elcomp/props
		       elcomp/ssa
		       elcomp/subst
		       elcomp/toplevel
		       elcomp/typeinf))
      (require feature))))
