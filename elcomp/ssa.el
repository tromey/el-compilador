;;; ssa.el --- change to SSA form. -*- lexical-binding:t -*-

;;; Commentary:

;; This is a simple and dumb approach to converting the function into
;; SSA form.  In particular it inserts way too many phi nodes, relying
;; on a later pass to prune them.

;; I think it would be better to replace this with an algorithm using
;; the dominance frontiers, though I haven't examined this too deeply.

;;; Code:

(require 'elcomp)
(require 'elcomp/eh-cleanup)

(defun elcomp--ssa-require-phis-for-block (_compiler bb)
  "Ensure that the `phis' slot for BB has been initialized."
  (unless (elcomp--basic-block-phis bb)
    (setf (elcomp--basic-block-phis bb) (make-hash-table))))

(defun elcomp--ssa-new-name (symbol)
  (cl-gensym (concat (symbol-name symbol) "_")))

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
	 (unless phi
	   (setf phi (elcomp--phi ;; FIXME "original-" is a misnomer
				  :original-name (elcomp--ssa-new-name name)))
	   (puthash name phi to-block-phis))
	 (puthash value t (elcomp--args phi))))
     current-map)))

(defun elcomp--ssa-note-lhs (insn current-map)
  "Note the left-hand side of an assignment.

If the left-hand-side of the assignment instruction INSN is
non-nil, then the instruction is added to CURRENT-MAP.

Returns t if CURRENT-MAP was updated, or nil if not."
  (let ((name (elcomp--sym insn)))
    (if name
	(let ((new-name (elcomp--ssa-new-name name)))
	  (setf (elcomp--sym insn) new-name)
	  (puthash name insn current-map)
	  t)
      nil)))

(defsubst elcomp--ssa-rename-arg (arg current-map)
  "Rename ARG using CURRENT-MAP."
  ;; FIXME - error if not found
  (gethash arg current-map arg))

(cl-defgeneric elcomp--ssa-rename (insn compiler current-map)
  "Update the instruction INSN to account for SSA renamings.

Operands of INSN are looked up in CURRENT-MAP and replaced.  If
INSN is an assignment, then the left-hand-side is also updated.

This returns t if CURRENT-MAP was modified by this renaming, and
nil otherwise.")

(cl-defmethod elcomp--ssa-rename ((insn elcomp--set) _compiler current-map)
  (setf (elcomp--value insn) (elcomp--ssa-rename-arg (elcomp--value insn)
						     current-map))
  (elcomp--ssa-note-lhs insn current-map))

(cl-defmethod elcomp--ssa-rename ((insn elcomp--call) _compiler current-map)
  ;; FIXME the :func slot.
  (let ((cell (elcomp--args insn)))
    (while cell
      (setf (car cell) (elcomp--ssa-rename-arg (car cell) current-map))
      (setf cell (cdr cell))))
  (elcomp--ssa-note-lhs insn current-map))

(cl-defmethod elcomp--ssa-rename ((insn elcomp--goto) compiler current-map)
  (elcomp--ssa-propagate compiler (elcomp--block insn) current-map)
  nil)

(cl-defmethod elcomp--ssa-rename ((insn elcomp--if) compiler current-map)
  (setf (elcomp--sym insn) (elcomp--ssa-rename-arg (elcomp--sym insn)
						   current-map))
  (elcomp--ssa-propagate compiler (elcomp--block-true insn) current-map)
  (elcomp--ssa-propagate compiler (elcomp--block-false insn) current-map)
  nil)

(cl-defmethod elcomp--ssa-rename ((insn elcomp--return) _compiler current-map)
  (setf (elcomp--sym insn) (elcomp--ssa-rename-arg (elcomp--sym insn)
						   current-map))
  nil)

(cl-defmethod elcomp--ssa-rename ((insn elcomp--return) _compiler current-map)
  (setf (elcomp--sym insn) (elcomp--ssa-rename-arg (elcomp--sym insn)
						   current-map))
  nil)

(defun elcomp--topmost-exception (bb)
  (catch 'done
    (dolist (topmost-exception (elcomp--basic-block-exceptions bb))
      (when (elcomp--handler topmost-exception)
	(throw 'done topmost-exception)))))

(defun elcomp--into-ssa-parse-args (compiler current-map)
  (let ((arg-list (cadr (elcomp--defun compiler))))
    (while arg-list
      (let ((this-arg (pop arg-list))
	    (is-rest nil))
	(cond
	 ((eq this-arg '&rest)
	  (setf is-rest t)
	  (setf this-arg (pop arg-list)))
	 ((eq this-arg '&optional)
	  (setf this-arg (pop arg-list))))
	(puthash this-arg (elcomp--argument :original-name this-arg
					    :is-rest is-rest)
		 current-map)))))

(defun elcomp--block-into-ssa (compiler bb)
  "Convert a single basic block into SSA form."
  (elcomp--ssa-require-phis-for-block compiler bb)
  ;; FIXME how to handle renaming for catch edges with a built-in
  ;; variable?  those variables are defined in that scope...
  (let ((current-map (copy-hash-table (elcomp--basic-block-phis bb))))
    ;; Set up the initial block with renamings of the arguments.
    (when (eq bb (elcomp--entry-block compiler))
      (elcomp--into-ssa-parse-args compiler current-map))
    (let ((changed-since-exception t)
	  (topmost-exception (elcomp--topmost-exception bb)))
      (dolist (insn (elcomp--basic-block-code bb))
	;; If this instruction can throw, and if there have been any
	;; changes since the last throwing instruction, then propagate
	;; any state changes to the exception handler.
	(when (and topmost-exception
		   changed-since-exception
		   (elcomp--can-throw insn))
	  (elcomp--ssa-propagate compiler (elcomp--handler topmost-exception)
				 current-map)
	  (setf changed-since-exception nil))
	;; Rename the operands and also see whether the map has
	;; changed.
	(when (elcomp--ssa-rename insn compiler current-map)
	  (setf changed-since-exception t))))))

(defun elcomp--into-ssa-pass (compiler)
  "A pass to convert the function in COMPILER into SSA form."
  (dolist (bb (elcomp--reverse-postorder compiler))
    (elcomp--block-into-ssa compiler bb)))

(provide 'elcomp/ssa)

;;; ssa.el ends here
