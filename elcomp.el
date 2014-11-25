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
  ;; A back link to the compilation unit.  This is needed so we can
  ;; push new functions into the compilation unit as we go.
  unit)

(cl-defstruct elcomp--compilation-unit
  ;; A hash table mapping a cons (a defun or a lambda) to a compiler
  ;; object.  If the value is t then the function hasn't been compiled
  ;; yet.
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
  ((sym :initform nil :initarg :sym)
   (value :initform nil :initarg :value)))

(defclass elcomp--call nil
  ((sym :initform nil :initarg :sym)
   (func :initform nil :initarg :func)
   (args :initform nil :initarg :args)))

(defclass elcomp--goto nil
  ((block :initform nil :initarg :block)))

(defclass elcomp--if nil
  ((sym :initform nil :initarg :sym)
   (block-true :initform nil :initarg :block-true)
   (block-false :initform nil :initarg :block-false)))

(defclass elcomp--return nil
  ((sym :initform nil :initarg :sym)))

(defclass elcomp--diediedie (elcomp--call)
  ()
  "An instruction which terminates a basic block without leading anywhere.

This can only be for a call to a `nothrow' function.")

;; An SSA operand representing a constant.
(defclass elcomp--constant nil
  ((value :initform nil :initarg :value)))

(defclass elcomp--phi nil
  ((original-name :initform nil :initarg :original-name)
   ;; Keys in this map are the possible source values for the PHI.
   ;; The values in the map are meaningless.
   (args :initform (make-hash-table) :initarg :args)))

(defclass elcomp--argument nil
  ((original-name :initform nil :initarg :original-name)
   (is-rest :initform nil :initarg :is-rest)))

;; An exception edge.
(defclass elcomp--exception nil
  ((handler :initform nil :initarg :handler)))

;; A catch.
(defclass elcomp--catch (elcomp--exception)
  ((tag :initform nil :initarg :tag)))

;; A single condition-case handler.
(defclass elcomp--condition-case (elcomp--exception)
  ((condition-name :initform nil :initarg :condition-name)))

;; An unwind-protect.
(defclass elcomp--unwind-protect (elcomp--exception)
  ;; The original form is used when optimizing "catch".
  ;; Well.. it will be someday.  FIXME.
  ((original-form :initform nil :initarg :original-form)))

;; A fake unwind-protect that is used to represent the unbind
;; operation from a `let' of a special variable.  This is needed to
;; properly deal with `catch' optimization from inside a `let', like:
;; (catch 'x (let* ((var1 (something)) (var2 (throw 'x 99))) ...))
;; Here, the `throw' has to unbind "var1".
(defclass elcomp--fake-unwind-protect (elcomp--exception)
  ((count :initform nil :initarg :count)))

(defun elcomp--ssa-name-p (arg)
  (or
   (elcomp--set-child-p arg)
   (elcomp--phi-child-p arg)
   (elcomp--call-child-p arg)
   (elcomp--argument-child-p arg)))

(defun elcomp--last-instruction (block)
  (car (elcomp--basic-block-code-link block)))

(gv-define-setter elcomp--last-instruction (val block)
  `(setcar (elcomp--basic-block-code-link ,block) ,val))

(defun elcomp--first-instruction (block)
  (car (elcomp--basic-block-code block)))

(gv-define-setter elcomp--first-instruction (val block)
  `(setcar (elcomp--basic-block-code ,block) ,val))

(defun elcomp--nonreturn-terminator-p (obj)
  (or (elcomp--goto-child-p obj)
      (elcomp--if-child-p obj)))

(defun elcomp--terminator-p (obj)
  (or (elcomp--goto-child-p obj)
      (elcomp--if-child-p obj)
      (elcomp--return-child-p obj)
      (elcomp--diediedie-child-p obj)))

(defun elcomp--any-hash-key (hash)
  "Return any key of the hash table HASH, or nil."
  (catch 'done
    (maphash (lambda (key _ignore) (throw 'done key)) hash)))

(provide 'elcomp)

;;; elcomp.el ends here
