;; Debugging the compiler.

(defgeneric elcomp--pp (obj stream)
  "FIXME")

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
  (princ (oref obj :args) stream))

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

;; Insert a single pretty-printed basic block into the current buffer.
(defun elcomp--pp-basic-block (bb)
  (insert (format "\n[Basic block %d]\n" (elcomp--basic-block-number bb)))
  (dolist (item (elcomp--basic-block-code bb))
    (elcomp--pp item (current-buffer))
    (insert "\n")))

;; Temporary function for hacking.
(defun elcomp--do (form)
  (let ((buf (get-buffer-create "*ELCOMP*")))
    (with-current-buffer buf
      (erase-buffer)
      (elcomp--iterate-over-bbs (elcomp--optimize form)
				#'elcomp--pp-basic-block))
    (pop-to-buffer buf)))
