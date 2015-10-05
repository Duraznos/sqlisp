(defparameter *x* (make-array 1024 :fill-pointer 0 :adjustable t :element-type 'character) "")

(defun parse-query (query)
  (format *query-io* "~a~%" query)
  (force-output *query-io*))

(defun repl-loop ()
  (let (line)
    (loop
       (setf line (read-line *query-io*))
       (if (equal line "exit") (return) (parse-query line) ))))

(defun input-test()
  (let (line)
    (setf line (read-line *query-io*))
    (if (equal line "exit") "yay" "boo")))

