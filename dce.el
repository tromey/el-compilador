;;; Dead Code Elimination.

(cl-defstruct elcomp--dce
  work-list
  (hash (make-hash-table))
  (just-intrinsic t))

(defgeneric elcomp--mark-necessary (insn dce)
  "Possibly mark the instruction INSN as necessary.
DCE is the DCE state object for the pass.

If the DCE 'just-intrinsic' slot is non-nil, then only mark the
instruction if it is intrinsically needed.  If it is nil, then
mark the instruction.

Marking the instruction means adding it to the hash and then
pushing the instruction's arguments onto the work list.")

(defmethod elcomp--mark-necessary (insn dce)
  ;; This is the base case -- most statements are always needed.
  (puthash insn t (elcomp--dce-hash dce)))

(defmethod elcomp--mark-necessary ((insn elcomp--return) dce)
  ;; An IF is always needed.
  (puthash insn t (elcomp--dce-hash dce))
  ;; And so is its referent.
  (push (oref insn :sym) (elcomp--dce-work-list dce)))

(defmethod elcomp--mark-necessary ((insn elcomp--return) dce)
  ;; A RETURN is always needed.
  (puthash insn t (elcomp--dce-hash dce))
  ;; And so is its referent.
  (push (oref insn :sym) (elcomp--dce-work-list dce)))

(defmethod elcomp--mark-necessary ((insn elcomp--set) dce)
  ;; A SET is not intrinsically needed, so check which pass this is.
  (unless (elcomp--dce-just-intrinsic dce)
    (puthash insn t (elcomp--dce-hash dce))
    (when (elcomp--ssa-name-p (oref insn :value))
      (push (oref insn :value) (elcomp--dce-work-list dce)))))

(defmethod elcomp--mark-necessary ((insn elcomp--phi) dce)
  ;; A PHI is not intrinsically needed, so check which pass this is.
  (unless (elcomp--dce-just-intrinsic dce)
    (puthash insn t (elcomp--dce-hash dce))
    (dolist (arg (oref insn :args))
      ;; FIXME - when would a PHI arg not be an SSA name?
      (when (elcomp--ssa-name-p arg)
	(push arg (elcomp--dce-work-list dce))))))

(defmethod elcomp--mark-necessary ((insn elcomp--call) dce)
  (let ((push-args nil))
    ;; A non-const call is intrinsically needed.  However, we mark it
    ;; specially so we can determine whether its LHS is needed as well.
    (if (elcomp--dce-just-intrinsic dce)
	(unless (elcomp--func-const-p (oref insn :func))
	  (puthash insn :call (elcomp--dce-hash dce))
	  (setf push-args t))
      ;; Otherwise, we're propagating.
      (puthash insn t (elcomp--dce-hash dce))
      (setf push-args t))
    (when push-args
      ;; Push the arguments on the work list.
      (dolist (arg (oref insn :args))
	(when (elcomp--ssa-name-p arg)
	  (push arg (elcomp--work-list-list work-list)))))))

(defun elcomp--dce-mark-intrinsically-necessary (compiler dce)
  (elcomp--iterate-over-bbs
   compiler
   (lambda (bb)
     (dolist (insn (elcomp--basic-block-code bb))
       (elcomp--mark-necessary insn dce)))))

(defun elcomp--dce-propagate-necessary (compiler dce)
  (setf (elcomp--dce-just-intrinsic dce) nil)
  (while (elcomp--dce-work-list dce)
    (let ((insn (pop (elcomp--dce-work-list dce)))
	  (mark (gethash (elcomp--dce-hash insn))))
      ;; If it is marked as 't', then we don't need to do any more.
      ;; If it is marked as :call, upgrade to 't'.
      (if mark
	  (when (eq mark :call)
	    ;; Upgrade a call.
	    (puthash insn t (elcomp--dce-hash dce)))
	(elcomp--mark-necessary insn dce)))))

(defun elcomp--dce-delete-dead-statements (compiler dce)
  (let ((hash elcomp--dce-hash dce))
    (elcomp--iterate-over-bbs
     compiler
     (lambda (bb)
       (let ((iter (elcomp--basic-block-code bb)))
	 (while iter
	   (unless (gethash (car iter) hash)
	     (setcar iter nil))
	   (setf iter (cdr iter))))
       (setf (elcomp--basic-block-code bb)
	     (delq nil (elcomp--basic-block-code bb)))))))

(defun elcomp--dce-pass (compiler)
  "Delete dead code."
  (let ((dce (make-elcomp--dce)))
    (elcomp--dce-mark-intrinsically-necessary compiler dce)
    (elcomp--dce-propagate-necessary compiler dce)
    (elcomp--dce-delete-dead-statements compiler dce)))
