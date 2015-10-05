
(defvar *db* nil)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun make-update-expr (field value)
  `(setf (getf row ,field) ,value))

(defun make-update-list (fields)
  (loop while fields
     collecting (make-update-expr (pop fields) (pop fields))))

(defmacro update (selector-fn &rest updates)
  `(setf *db*
	 (mapcar
	  #'(lambda (row)
	      (when (funcall ,selector-fn row)
		,@(make-update-list updates))
		row) *db*)))

