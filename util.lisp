(defpackage #:cl-telegram-util
  (:use :cl)
  (:export
   #:subthread)
  (:export
   #:with-task-return-type
   #:make-task-return-type)
  (:export
   #:get-json-object-member
   #:get-json-object-members*)
  (:export
   #:while))
(in-package #:cl-telegram-util)

(defun subthread (fun)
  "Run a function in a sub-thread"
  (bt:make-thread fun))

;;A task-return-type is of the format (type &optional value)
(defmacro with-task-return-type ((type-var value-var) value-form &body body)
  `(destructuring-bind (,type-var &rest ,value-var) ,value-form
     ,@body))
(defun make-task-return-type (type &optional data)
  (cons type data))


(progn
  (defun get-json-object-member (object key)
    "Like jsown:val, but returns t or nil on success or failure rather
than raising an error. jsown:val accesses the variables of a json object."
    (block out
      (handler-bind
	  ((simple-error
	    (lambda (c)
	      (declare (ignorable c))
	      (return-from out (values nil nil)))))
	(values
	 (jsown:val object key)
	 t))))
  (defun get-json-object-members* (object key-list)
    ;;navivate the json object as if it were a tree/directory
    (block out
      (dolist (key key-list)
	(multiple-value-bind (inner-object existsp) (get-json-object-member object key)
	  (cond (existsp
		 (setf object inner-object))
		(t
		 (print key)
		 (return-from out (values nil nil))))))
      (values object t))))


;;tests:
;;FIXME::put somewhere? a test file?
#+nil
(progn
  (assert (equal (get-json-object-member '(:obj ("foo" :bar)) "foo")
		 '(:bar)))
  (assert (equal (get-json-object-members*
		  '(:obj ("foo" . (:obj
				   ("frob" 37)
				   ("baz" . (:obj ("qux" . :bar))))))
		  (list "foo" "baz" "qux"))
		 ':bar)))

(defmacro while ((clause) &body body)
  `(do ()
       ((not ,clause))
     ,@body))
