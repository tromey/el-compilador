;;; elcomp.el - Compiler for Emacs Lisp. -*- lexical-binding:t -*-

;;; Commentary:

;; This holds basic definitions for the compiler.  Everything else is
;; in the elcomp subdir.

;;; Code:

(require 'cl-macs)
(require 'eieio)

(cl-defstruct (elcomp (:conc-name elcomp--))
  ;; An alist holding symbol rewrites.  The car of each element is a
  ;; symbol in the original code.  The cdr is the symbol to which it
  ;; is rewritten.
  rewrite-alist
  ;; Next label value.
  (next-label 0) 
  ;; The entry block.
  entry-block
  ;; The current basic block.
  current-block
  ;; True if the back-edges in the CFG are considered valid.
  ;; FIXME - deal with IDOM being invalid too
  back-edges-valid
  ;; The current list of exception handlers.
  exceptions
  ;; The current defun being compiled.
  ;; This is a list (NAME ARGLIST DOC INTERACTIVE).
  ;; NAME is nil for an anonymous function.
  ;; FIXME this should just be separate slots of this struct.
  defun
  ;; The name of the defun, a symbol.  This must be computed using
  ;; elcomp--get-name, as this is either set lazily from 'defun', or
  ;; generated for lambdas.
  name
  ;; A back link to the compilation unit.  This is needed so we can
  ;; push new functions into the compilation unit as we go.
  unit)

(cl-defstruct elcomp--compilation-unit
  ;; A hash table mapping a cons (a defun or a lambda) to a compiler
  ;; object.
  (defuns (make-hash-table))
  ;; The work-list.  This is separate from `defuns' for convenience.
  work-list)

(cl-defstruct elcomp--basic-block
  ;; Block number.
  number
  ;; The code for this basic block.
  code
  ;; Last link of linearized code.
  code-link
  ;; A hash table holding back-links to parent nodes.
  ;; Outgoing edges are represented directly by the last instruction
  ;; in the code sequence.
  parents
  ;; The immediate dominator, or nil if not known.
  immediate-dominator
  ;; The list of exception handlers.
  exceptions
  ;; The phi nodes for this basic block.  This is a hash table whose
  ;; keys are original variable names and whose values are phis.  This
  ;; starts as nil and is initialized when converting to SSA form.
  phis
  ;; Final type map for this BB.
  final-type-map
  ;; Entry type map for this BB.  This is not needed after type
  ;; inferencing.  FIXME store on the side.
  type-map)

(defclass elcomp--set nil
  ((sym :initform nil :initarg :sym
	:documentation "The local variable being assigned to.
Initially this is a symbol.
After transformation to SSA, this will be an SSA name;
see `elcomp--ssa-name-p'.")
   (value :initform nil :initarg :value
	  :documentation "The value being assigned.
Initially this is a symbol.
After transformation to SSA, this will be an SSA name."))
  "A `set' instruction.

This represents a simple assignment to a local variable.")

(defclass elcomp--call nil
  ((sym :initform nil :initarg :sym
	:documentation "The local variable being assigned to.
This can be `nil' if the result of the call is not used.
Initially this is a symbol.
After transformation to SSA, this will be an SSA name;
see `elcomp--ssa-name-p'.")
   (func :initform nil :initarg :func
	 :accessor elcomp--func
	 :documentation "The function to call.
This may be a symbol or a `lambda' list.")
   (args :initform nil :initarg :args
	 :accessor elcomp--args
	 ;; FIXME - can a symbol wind up in here or do we make
	 ;; symbol-value explicit?
	 :documentation "The arguments to the function.
Initially this is a list of symbols.
After transformation to SSA, this will be a list of SSA names."))
  "A function call instruction.")

(defclass elcomp--goto nil
  ((block :initform nil :initarg :block
	  :accessor elcomp--block
	  :documentation "The target block."))
  "A `goto' instruction.
This instruction terminates a block.")

(defclass elcomp--if nil
  ((sym :initform nil :initarg :sym
	:documentation "The condition to check.
Initially this is a symbol.
After transformation to SSA, this will be an SSA name;
see `elcomp--ssa-name-p'.")
   (block-true :initform nil :initarg :block-true
	       :accessor elcomp--block-true
	       :documentation "The target block if the value is non-`nil'.")
   (block-false :initform nil :initarg :block-false
		:accessor elcomp--block-false
		:documentation "The target block if the value is `nil'."))
  "An `if' instruction.
This branches to one of two blocks based on whether or not the
argument is `nil'.  This instruction terminates a block.")


(defclass elcomp--return nil
  ((sym :initform nil :initarg :sym
	:documentation "The value to return.
Initially this is a symbol.
After transformation to SSA, this will be an SSA name;
see `elcomp--ssa-name-p'."))
  "A `return' instruction.")

(defclass elcomp--diediedie (elcomp--call)
  ()
  "An instruction which terminates a basic block without leading anywhere.

This can only be for a call to a `nothrow' function.")

(defclass elcomp--constant nil
  ((value :initform nil :initarg :value
	  :documentation "The value of the constant."))
  "This represents a constant after transformation to SSA form.")

(defclass elcomp--phi nil
  ((original-name :initform nil :initarg :original-name
		  :documentation "The original name of this node.
This is handy for debugging.")
   (args :initform (make-hash-table) :initarg :args
	 :accessor elcomp--args
	 :documentation "Arguments to this node.
This is a hash table whose keys are possible source values for the phi.
The values in the hash table are meaningless."))
  "A `phi' node.

See any good source of information about SSA to understand this.")

(defclass elcomp--argument nil
  ((original-name :initform nil :initarg :original-name
		  :documentation "The original name of this node.
This is handy for debugging.")
   (is-rest :initform nil :initarg :is-rest
	    :accessor elcomp--is-rest
	    :documentation "True if this argument was from `&rest'."))
  "A function argument.  This is only used in SSA form.")

(defclass elcomp--exception nil
  ((handler :initform nil :initarg :handler
	    :accessor elcomp--handler
	    :documentation "The target block of this exception edge."))
  "An exception edge.

A block's `exceptions' slot is a list of all the active exception
handlers, though in most cases only the first one is ever
taken.")

(defclass elcomp--catch (elcomp--exception)
  ((tag :initform nil :initarg :tag
	:documentation "The tag of the `catch'."))
  "An exception edge representing a `catch'.")

(defclass elcomp--condition-case (elcomp--exception)
  ((condition-name :initform nil :initarg :condition-name
		   :documentation "The name of the condition being handled.

This is either a symbol or nil.  Note that the variable that can
be bound by `condition-case' is explicit in the target block."))
  "An exception edge representing a single `condition-case' handler.")

(defclass elcomp--unwind-protect (elcomp--exception)
  ;; The original form is used when optimizing "catch".
  ;; Well.. it will be someday.  FIXME.
  ((original-form :initform nil :initarg :original-form
		  :documentation "The original form.
This is not used now but may be later for `catch' optimization."))
  "An exception edge representing an `unwind-protect'.")

;; A fake unwind-protect that is used to represent the unbind
;; operation from a `let' of a special variable.  This is needed to
;; properly deal with `catch' optimization from inside a `let', like:
;; (catch 'x (let* ((var1 (something)) (var2 (throw 'x 99))) ...))
;; Here, the `throw' has to unbind "var1".
(defclass elcomp--fake-unwind-protect (elcomp--exception)
  ((count :initform nil :initarg :count
	  :documentation "The number of unbinds that this represents."))
  "An exception edge representing the unbind operation from a `let'
of a special variable.  These unbinds are done implicitly, so this
exception edge does not represent any ordinary code -- but it is needed
to properly deal do the `catch' optimization from inside a `let', like:

    (catch 'x (let* ((var1 (something)) (var2 (throw 'x 99))) ...))

Here, the `throw' has to unbind `var1'.")

(defun elcomp--ssa-name-p (arg)
  "Return t if ARG is an SSA name."
  (or
   (elcomp--set-p arg)
   (elcomp--phi-p arg)
   (elcomp--call-p arg)
   (elcomp--argument-p arg)))

(defun elcomp--last-instruction (block)
  "Return the last instruction in BLOCK.

This can be used with `setf'."
  (car (elcomp--basic-block-code-link block)))

(gv-define-setter elcomp--last-instruction (val block)
  `(setcar (elcomp--basic-block-code-link ,block) ,val))

(defun elcomp--first-instruction (block)
  "Return the first instruction in BLOCK.

This can be used with `setf'."
  (car (elcomp--basic-block-code block)))

(gv-define-setter elcomp--first-instruction (val block)
  `(setcar (elcomp--basic-block-code ,block) ,val))

(defun elcomp--nonreturn-terminator-p (obj)
  "Return t if OBJ is a block-terminating instruction other than
`return' or `diediedie'."
  (or (elcomp--goto-p obj)
      (elcomp--if-p obj)))

(defun elcomp--terminator-p (obj)
  "Return t if OBJ terminates a block."
  (or (elcomp--goto-p obj)
      (elcomp--if-p obj)
      (elcomp--return-p obj)
      (elcomp--diediedie-p obj)))

(defun elcomp--any-hash-key (hash)
  "Return any key of the hash table HASH, or nil."
  (catch 'done
    (maphash (lambda (key _ignore) (throw 'done key)) hash)))

(defun elcomp--get-name (elcomp)
  "Get the name of the function represented by ELCOMP."
  (unless (elcomp--name elcomp)
    (setf (elcomp--name elcomp)
	  (if (car (elcomp--defun elcomp))
	      (car (elcomp--defun elcomp))
	    (cl-gensym "__lambda"))))
  (elcomp--name elcomp))

(provide 'elcomp)

;;; elcomp.el ends here
