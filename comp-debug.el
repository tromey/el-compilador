;; Debugging the compiler.

(defgeneric elcomp--pp (obj stream)
  "FIXME")

(defmethod elcomp--pp (obj stream)
  (error "unrecognized instruction"))

;; FIXME eldoc for defmethod is messed up
(defmethod elcomp--pp ((obj elcomp--set) stream)
  (princ "set " stream)
  (princ (oref obj :sym) stream)
  (princ " = " stream)
  (princ (oref obj :value) stream))

(defmethod elcomp--pp ((obj elcomp--call) stream)
  (princ "call " stream)
  (princ (oref obj :sym) stream)
  (princ " = " stream)
  (princ (oref obj :func) stream)
  (when (oref obj :args)
    (princ (oref obj :args) stream)))

(defmethod elcomp--pp ((obj elcomp--goto) stream)
  (princ "goto BB " stream)
  (princ (elcomp--basic-block-number (oref obj :block)) stream))

(defmethod elcomp--pp ((obj elcomp--if) stream)
  (princ "if " stream)
  (princ (oref obj :sym) stream)
  (princ " BB " stream)
  (princ (elcomp--basic-block-number (oref obj :block-true)) stream)
  (princ " else BB " stream)
  (princ (elcomp--basic-block-number (oref obj :block-false)) stream))

(defmethod elcomp--pp ((obj elcomp--return) stream)
  (princ "return " stream)
  (princ (oref obj :sym) stream))

(defmethod elcomp--pp ((obj elcomp--constant) stream)
  (princ "<< ")
  (princ (oref obj :value) stream)
  (princ " >>"))

(defmethod elcomp--pp ((obj elcomp--phi) stream)
  (princ "phi " stream)
  (princ (oref obj :sym) stream)
  (princ " =" stream)
  (dolist (item (oref obj :args))
    (princ " " stream)
    (elcomp--pp item stream)))

(defmethod elcomp--pp ((obj elcomp--catch) stream)
  (princ "catch " stream)
  (princ (oref obj :result) stream)
  (princ " = " stream)
  (princ (oref obj :tag) stream)
  (princ " => BB " stream)
  (princ (elcomp--basic-block-number (oref obj :handler)) stream))

(defmethod elcomp--pp ((obj elcomp--condcase) stream)
  (princ "condition-case " stream)
  (princ (oref obj :variable) stream)
  (princ ", " stream)
  (princ (oref obj :condition-name) stream)
  (princ " => BB " stream)
  (princ (elcomp--basic-block-number (oref obj :handler)) stream))

(defmethod elcomp--pp ((obj elcomp--unwind-protect) stream)
  (princ "unwind-protect => BB " stream)
  (princ (elcomp--basic-block-number (oref obj :handler)) stream))

;; Insert a single pretty-printed basic block into the current buffer.
(defun elcomp--pp-basic-block (bb)
  (insert (format "\n[BB %d"
		  (elcomp--basic-block-number bb)))
  (when (> (hash-table-count (elcomp--basic-block-parents bb)) 0)
    (insert " (parents:")
    (maphash (lambda (parent-bb _ignore)
	       (insert (format " %d" (elcomp--basic-block-number parent-bb))))
	     (elcomp--basic-block-parents bb))
    (insert ")"))
  (insert (format " (idom: %d)"
		  (elcomp--basic-block-number
		   (elcomp--basic-block-immediate-dominator bb))))
  (insert "]\n")
  (dolist (exception (elcomp--basic-block-exceptions bb))
    (insert "    ")
    (elcomp--pp exception (current-buffer))
    (insert "\n"))
  (dolist (item (elcomp--basic-block-code bb))
    (elcomp--pp item (current-buffer))
    (insert "\n")))

;; Temporary function for hacking.
(defun elcomp--do (form)
  (let ((buf (get-buffer-create "*ELCOMP*")))
    (with-current-buffer buf
      (erase-buffer)
      (elcomp--iterate-over-bbs (elcomp--translate form)
				#'elcomp--pp-basic-block))
    (pop-to-buffer buf)))
