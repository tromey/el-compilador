;;; Jump-threading pass.

(defun elcomp--thread-jumps-pass (compiler)
  (let ((rewrote-one t))
    (while rewrote-one
      (setf rewrote-one nil)
      (elcomp--iterate-over-bbs
       compiler
       (lambda (block)
	 (let ((insn (elcomp--last-instruction block)))
	   ;; A GOTO to a block holding just another branch (of any kind)
	   ;; can be replaced by the instruction at the target.
	   (when (and (elcomp--goto-child-p insn)
		      (elcomp--nonreturn-terminator-p
		       (elcomp--first-instruction (oref insn :block))))
	     (setf (elcomp--last-instruction block)
		   (elcomp--first-instruction (oref insn :block)))
	     (setf rewrote-one t))

	   ;; If a target of an IF is a GOTO, the destination can be
	   ;; hoisted.
	   (when (and (elcomp--if-child-p insn)
		      (elcomp--goto-child-p (elcomp--first-instruction
					     (oref insn :block-true))))
	     (oset insn :block-true
		   (oref (elcomp--first-instruction (oref insn :block-true))
			 :block))
	     (setf rewrote-one t))
	   (when (and (elcomp--if-child-p insn)
		      (elcomp--goto-child-p (elcomp--first-instruction
					     (oref insn :block-false))))
	     (oset insn :block-false
		   (oref (elcomp--first-instruction (oref insn :block-false))
			 :block))
	     (setf rewrote-one t))

	   ;; If both branches of an IF point to the same spot, turn
	   ;; it into a GOTO.
	   (when (and (elcomp--if-child-p insn)
		      (eq (oref insn :block-true)
			  (oref insn :block-false)))
	     (setf (elcomp--last-instruction block)
		   (elcomp--goto "goto" :block (oref insn :block-true)))
	     (setf rewrote-one t))

	   ;; If a target of an IF is another IF, and the conditions are the
	   ;; same, then the target IF can be hoisted.
	   (when (elcomp--if-child-p insn)
	     ;; Thread the true branch.
	     (when (and (elcomp--if-child-p (elcomp--first-instruction
					     (oref insn :block-true)))
			(eq (oref insn :sym)
			    (oref (elcomp--first-instruction
				   (oref insn :block-true))
				  :sym)))
	       (oset insn :block-true
		     (oref (elcomp--first-instruction
			    (oref insn :block-true))
			   :block-true)))
	     ;; Thread the false branch.
	     (when (and (elcomp--if-child-p (elcomp--first-instruction
					     (oref insn :block-false)))
			(eq (oref insn :sym)
			    (oref (elcomp--first-instruction
				   (oref insn :block-false))
				  :sym)))
	       (oset insn :block-false
		     (oref (elcomp--first-instruction
			    (oref insn :block-false))
			   :block-false))))))))))
