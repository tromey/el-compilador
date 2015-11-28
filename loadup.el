(defconst elcomp--loadup-dir
  (file-name-directory (or load-file-name
			   ;; In the eval-buffer case.
			   (buffer-file-name))))

(defun elcomp--loadup ()
  (interactive)
  (let ((load-path load-path))
    (push elcomp--loadup-dir load-path)
    (dolist (file '("elcomp"
		    "elcomp/back"
		    "elcomp/c-inl"
		    "elcomp/cmacros"
		    "elcomp/coalesce"
		    "elcomp/comp-debug"
		    "elcomp/cprop"
		    "elcomp/dce"
		    "elcomp/dom"
		    "elcomp/eh-cleanup"
		    "elcomp/eltoc"
		    "elcomp/iter"
		    "elcomp/jump-thread"
		    "elcomp/linearize"
		    "elcomp/name-map"
		    "elcomp/props"
		    "elcomp/ssa"
		    "elcomp/subst"
		    "elcomp/toplevel"
		    "elcomp/typeinf"))
      (load file))))
