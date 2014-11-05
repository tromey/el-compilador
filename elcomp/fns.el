(defun identity (arg)
  "Return the argument unchanged."
  arg)

(defun copy-alist (alist)
  "Return a copy of ALIST.
This is an alist which represents the same mapping from objects to objects,
but does not share the alist structure with ALIST.
The objects mapped (cars and cdrs of elements of the alist)
are shared, however.
Elements of ALIST that are not conses are also shared."
  (cl-check-type alist list)
  (mapcar #'identity alist))

(defun nthcdr (num list)
  (cl-check-type n integer)
  (let ((i 0))
    (while (and (< i num) list)
      (setq list (cdr list))
      (setq i (1+ i)))
    list))

(defun nth (n list)
  (car (nthcdr n list)))

(defun elt (sequence n)
  (cl-check-type n integer)
  (if (listp sequence)
      (car (nthcdr n sequence))
    (aref sequence n)))

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT."
  (let ((tail list)
	(result nil))
    (while (and (not result) (consp tail))
      (let ((tem (car tail)))
	(if (equal elt tem)
	    (setq result elt))))
    result))

(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT."
  (let ((tail list)
	(result nil))
    (while (and (not result) (consp tail))
      (let ((tem (car tail)))
	(if (eq elt tem)
	    (setq result elt))))
    result))

(defun memql (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eql'.
The value is actually the tail of LIST whose car is ELT."
  (let ((tail list)
	(result nil))
    (while (and (not result) (consp tail))
      (let ((tem (car tail)))
	(if (eql elt tem)
	    (setq result elt))))
    result))

(defun assq (key list)
  "Return non-nil if KEY is `eq' to the car of an element of LIST.
The value is actually the first element of LIST whose car is KEY.
Elements of LIST that are not conses are ignored."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (eq (car (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
  (car list))

(defun assoc (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the first element of LIST whose car is KEY."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (equal (car (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
  (car list))

(defun rassq (key list)
  "Return non-nil if KEY is `eq' to the cdr of an element of LIST.
The value is actually the first element of LIST whose cdr is KEY."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (eq (cdr (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
  (car list))

(defun rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.b
The value is actually the first element of LIST whose cdr is KEY."
  (let ((keep-going t))
    (while (and keep-going (consp list))
      (if (and (consp (car list))
	       (equal (cdr (car list)) key))
	  (setq keep-going nil)
	(setq list (cdr list)))))
  (car list))

(defun nreverse (list)
  "Reverse LIST by modifying cdr pointers.
Return the reversed list.  Expects a properly nil-terminated list."
  (let ((prev nil)
	(tail list))
    (while tail
      (let ((next (cdr tail)))
	(setcdr tail prev)
	(setq prev tail)
	(setq tail next)))
    prev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun featurep (feature &optional subfeature)
  (check-type feature symbol)
  (let ((tem (memq feature features)))
    (and tem subfeature
	 (setq tem (member subfeature (get feature 'subfeatures))))
    (if tem t)))

(defun provide (feature subfeatures)
  (check-type feature symbol)
  (check-type subfeatures list)
  (if autoload-queue
      (push (cons 0 features) autoload-queue))
  (unless (memq feature features)
    (push feature features))
  (if subfeatures
      (put feature 'subfeatures subfeatures))
  ;; LOADHIST_ATTACH (Fcons (Qprovide, feature));
  (let ((tem (assq feature after-load-alist)))
    (if (consp tem)
	(mapc #'funcall (cdr tem))))
  feature)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plist-member (plist prop)
  (while (and (consp plist)
	      (not (eq (car plist) prop)))
    (setf plist (cddr plist)))
  plist)

(defun widget-put (widget property value)
  (setcdr widget (plist-put (cdr widget) property value)))

(defun widget-get (widget property)
  (catch 'done				;FIXME - lexical catch
    (while t
      (unless widget
	(throw 'done nil))
      (let ((tmp (plist-member (cdr widget) property)))
	(when (consp tmp)
	  (setf tmp (cdr tmp))
	  (throw 'done (car tmp)))
	(setf tmp (car widget))
	(unless tmp
	  (throw 'done nil))
	(setf widget (get tmp 'widget-type))))))

(defun widget-apply (widget property &rest args)
  (apply (widget-get widget property) widget args))
