;;; ssa.el --- change to SSA form.

;;; Commentary:

;; This is a simple and dumb approach to converting the function into
;; SSA form.  In particular it inserts way too many phi nodes, relying
;; on a later pass to prune them.

;;; Code:

(defun elcomp--ssa-require-phis-for-block (compiler bb)
  "Ensure that the `phis' slot for BB has been initialized."
  (unless (elcomp--basic-block-phis bb)
    (setf (elcomp--basic-block-phis bb) (make-hash-table))
    (dolist (var-name (elcomp--basic-block-entry-vars))
      (puthash var-name (elcomp--phi "phi" :original-name var-name)))
    ;; We don't need this any more.
    (setf (elcomp--basic-block-entry-vars bb) nil)))

(defun elcomp--ssa-propagate (compiler to-block current-map)
  "Propagate name mappings for phi nodes.

This adds the name mappings in CURRENT-MAP to the incoming name
map of TO-BLOCK.  All this does is add the incoming mappings to
the existing phi nodes."
  (elcomp--ssa-require-phis-for-block compiler to-block)
  (let ((to-block-phis (elcomp--basic-block-phis to-block)))
    (maphash
     (lambda (name value)
       (let ((phi (gethash name to-block-phis)))
	 (cl-assert phi)
	 (puthash value t (oref phi :args))))
     current-map)))

(defun elcomp--ssa-rename-lhs (compiler bb insn current-map)
  "Rename the left-hand side of an assignment.

If the left-hand-side of the assignment instruction INSN is
non-nil, then a new variable is generated and added to
CURRENT-MAP.  INSN is modified to reflect the new name."
  (let ((name (oref insn :sym)))
    (if name
	(let ((new-name (elcomp--ssa-variable "ssa-name" :original-name name)))
	  (puthash name new-name current-map)
	  (setf (oref insn :sym) new-name)
	  t)
      nil)))

(defsubst elcomp--ssa-rename-arg (arg current-map)
  "Rename ARG using CURRENT-MAP."
  ;; FIXME - error if not found
  (gethash arg current-map arg))

(defgeneric elcomp--ssa-rename (insn compiler bb current-map)
  "Update the instruction INSN to account for SSA renamings.

Operands of INSN are looked up in CURRENT-MAP and replaced.  If
INSN is an assignment, then the left-hand-side is also updated.

This returns t if CURRENT-MAP was modified by this renaming, and
nil otherwise.")

(defmethod elcomp--ssa-rename ((insn elcomp--set) compiler bb current-map)
  (setf (oref insn :value) (elcomp--ssa-rename-arg (oref insn :value)
						   current-map))
  (elcomp--ssa-rename-lhs compiler bb insn current-map))

(defmethod elcomp--ssa-rename ((insn elcomp--call) compiler bb current-map)
  ;; FIXME the :func slot.
  (let ((cell (oref insn :args)))
    (while cell
      (setf (car cell) (elcomp--ssa-rename-arg (car cell) current-map))
      (setf cell (cdr cell))))
  (elcomp--ssa-rename-lhs compiler bb insn current-map))

(defmethod elcomp--ssa-rename ((insn elcomp--goto) compiler bb current-map)
  (elcomp--ssa-propagate compiler (oref insn :block) current-map)
  nil)

(defmethod elcomp--ssa-rename ((insn elcomp--if) compiler bb current-map)
  (setf (oref insn :sym) (elcomp--ssa-rename-arg (oref insn :sym) current-map))
  (elcomp--ssa-propagate compiler (oref insn :block-true) current-map)
  (elcomp--ssa-propagate compiler (oref insn :block-false) current-map)
  nil)

(defmethod elcomp--ssa-rename ((insn elcomp--return) compiler bb current-map)
  (setf (oref insn :sym) (elcomp--ssa-rename-arg (oref insn :sym) current-map))
  nil)

(defmethod elcomp--ssa-rename ((insn elcomp--return) compiler bb current-map)
  (setf (oref insn :sym) (elcomp--ssa-rename-arg (oref insn :sym) current-map))
  nil)

(defun elcomp--block-into-ssa (compiler bb)
  "Convert a single basic block into SSA form."
  (elcomp--ssa-require-phis-for-block compiler bb)
  ;; FIXME how to handle renaming for catch edges with a built-in
  ;; variable?  those variables are defined in that scope...
  (let ((current-map (copy-hash-table (elcomp--basic-block-phis bb))))
    (let ((changed-since-exception nil)
	  (topmost-exception (elcomp--basic-block-exceptions bb)))
      (dolist (insn (elcomp--basic-block-code bb))
	;; Propagate any state changes to the exception handler.  Note
	;; that `changed-since-exception' can be initialized to nil
	;; because an exception can't have been thrown yet.  FIXME?
	(when (and topmost-exception changed-since-exception)
	  (elcomp--ssa-propagate current-map
				 (oref topmost-exception :handler))
	  (setf changed-since-exception nil))
	;; Rename the operands and also see whether the map has
	;; changed.
	(when (elcomp--ssa-rename insn compiler bb current-map)
	  (setf changed-since-exception t))))))

(defun elcomp--into-ssa-pass (compiler)
  "A pass to convert the function in COMPILER into SSA form."
  (elcomp--iterate-over-bbs compiler #'elcomp--block-into-ssa))

;;; ssa.el ends here
