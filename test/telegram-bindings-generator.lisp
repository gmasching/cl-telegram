(defpackage #:telegram-bindings-generator
  (:use :cl))
(in-package :telegram-bindings-generator)

(defparameter *this-directory* (merge-pathnames "test/" (asdf:system-source-directory :cl-telegram)))
(defparameter *html-file-path*
  (merge-pathnames "Telegram_Bot_API.html"
		   *this-directory*))

(defparameter *file* (alexandria:read-file-into-string *html-file-path*))
(defparameter *root* (plump:parse *file*))

(defparameter *data0*
  (plump:get-element-by-id *root* "dev_page_content"))
#+nil
(ql:quickload :plump)

;;find h4 tags in *data0*

(defparameter *children-elements* (plump:child-elements *data0*))

(defun tag-type= (string node)
  (and
   (typep node 'plump:node)
   (string= string (plump:tag-name node))))

(defun separate-h4s (&optional (seq *children-elements*))
  (let ((acc)
	(top))
    (flet ((new (item)
	     (push item top))
	   (new-stack ()
	     (push (nreverse top) acc)
	     (setf top nil)))
      (loop for node across seq
	 do (progn
	      (when (or (tag-type= "h4" node)
			(tag-type= "h3" node))
		(new-stack))
	      (new node))))
    (nreverse acc)))
(defparameter *data1* 
  (separate-h4s))
;;The h4 headings seem to contain the data
(defparameter *data2* (remove-if-not (lambda (x) (tag-type= "h4" (first x)))
				     *data1*))
(defun nothing ()
  (mapcar
   (lambda (list0)
     (mapcar (lambda (node)
	       (cond (;;extract the title from the h4 node
		      (or (tag-type= "h4" node)
			  (tag-type= "p" node)
			  (tag-type= "blockquote" node))
		      (plump:text node))
		     (t node)))
	     (remove-if
	      (lambda (x)
		(tag-type= "blockquote" x))
	      list0)))
   *data2*))
(defparameter *data3* (nothing))

;;functions are likeThis and types are LikeThis
(defun remove-not-functions-or-objects (&optional (list *data3*))
  (remove-if
   (lambda (x)
     (find #\Space (first x)))
   list))

(defparameter *data4* (remove-not-functions-or-objects))


#+nil
(progn
  ;;remove things that are not strings or tables
  (defun remove-other-things (&optional (list *data4*))
    (mapcar
     (lambda (x)
       (remove-if-not
	(lambda (x) (or (stringp x)
			(tag-type= "table" x)))
	x))
     list))

  (defparameter *data5* (remove-other-things)))
;;LikeThis
(defparameter *types*
  (remove-if-not (lambda (x)
		   (upper-case-p (aref (first x) 0)))
		 *data4*))
;;likeThis
;;currently containts either nil->no arguments or a single table
(defparameter *functions*
  (remove-if-not (lambda (x)
		   (lower-case-p (aref (first x) 0)))
		 *data4*))

(defun stringless (list)
  (mapcar 'data
	  list))

(defun longest (seq)
  (find (reduce 'max seq :key 'length) seq :key 'length
	))

(defun description (item)
  (nice-concatenate
   (remove-if-not 'stringp (cdr item))))

(defun data (item) (remove-if 'stringp (cdr item)))

(defun stringfulthing (list)
  (mapcar 'description
	  list))

(defun nice-concatenate (list)
  (with-output-to-string (str)
    (dolist (item list)
      (princ item str)
      (terpri str))))

;;ul vs table tag
;;exception::
;;ForceReply

(defun nicer-order-function (&optional (list *functions*))
  (mapcar (lambda (x)
	    (list (car x)
		  (let ((item (first (data x))))
		    (cond (;;The table indicates the parameters of the function
			   (tag-type= "table" item)
			   (list :parameters (convert-table item)))
			  ((null item) :no-parameters) ;;means no parameters
			  (t (error "what is this datum? for a function API ~a" item))))
		  (description x)))
	  list))

;;;for types
(defun nicer-order-type (&optional (list *types*))
  (mapcar (lambda (x)
	    (list (car x)
		  (let ((item (first (data x))
			  ;;FIXME::only coincidentally works? There is ForceReply
			  ;;That has an ul and a table
			  ))
		    (cond (;;The table indicates how the object type is layed out
			   (tag-type= "table" item)
			   (list :layout (convert-table item)))
			  ((null item) :placeholder) ;; a placeholder for types
			  ((tag-type= "ul" item)
			   (list :enum (convert-ul item)))
			  (t (error "what is this datum? for a type API ~a" item))))
		  (description x)))
	  list))

(defparameter *table-test* (car (first (stringless *functions*))))

(defun convert-table (&optional (node *table-test*))
  (mapcar
   (lambda (node)
     (mapcar
      (lambda (node)
	(mapcar
	 (lambda (node)	   
	   (plump:text node))
	 (coerce
	  (plump:child-elements node)
	  'list)))
      (coerce
       (plump:child-elements node)
       'list)))
   (coerce
    (plump:child-elements node)
    'list)))

(defun test678 ()
  (mapcar 'convert-table (remove nil (mapcar 'first (stringless *functions*)))))

(defun convert-ul (node)
  (map 'list 'plump:text
       (plump:child-elements node)))

(defparameter *uls* (mapcar 'first (remove-if (lambda (x) (tag-type= "table" (first x))) (remove nil (stringless *types*)))))

(defparameter *nice-types* (nicer-order-type))
(defparameter *nice-functions* (nicer-order-function))

;;The ordering for function parameter tables
#+nil
("Parameter" "Type" "Required" "Description")
(defun function-parameter-table-uniformity-test ()
  (= 1
     (length
      ;;if they are all the same, there should be one left
      (remove-duplicates
       ;;get the table header
       (print ;;<-This is here, just saying
	(mapcar 'first
		;;nil removed
		(remove nil
			(mapcar (lambda (x)
				  (if (typep (second x)
					     ;;HACK::get only those items that have a parameters table,
					     ;;nil otherwise
					     '(cons (eql :parameters)))
				      (second (second x))
				      nil))
				*nice-functions*))))
       :test 'equalp))))
;;These two formats are only coincidentally the same, so we have two functions
;;The ordering for type layouts
#+nil
("Field" "Type" "Description")
(defun type-layout-table-uniformity-test ()
  (= 1
     (length
      ;;if they are all the same, there should be one left
      (remove-duplicates
       ;;get the table header
       (mapcar 'first
	       ;;nil removed
	       (remove nil
		       (mapcar (lambda (x)
				 (if (typep (second x)
					    ;;HACK::get only those items that have a parameters table,
					    ;;nil otherwise
					    '(cons (eql :layout)))
				     (second (second x))
				     nil))
			       *nice-types*)))
       :test 'equalp))))

;;(name args description)
;;args is not :no-parameters
(defun even-nicer-functions ()
  (mapcar (lambda (x)
	    (destructuring-bind (name table-thing description) x
	      (list name
		    (etypecase table-thing
		      ((cons (eql :parameters))
		       ;;getting the data, tossing the header, which is redundant
		       ;;and also tossing the label ':parameters
		       (second (second table-thing)))
		      ((eql :no-parameters) nil))
		    description)))
	  *nice-functions*))

(defun consolidate (x)
  (remove-duplicates x :test 'equalp))
(defun functions-types-stuff ()
  (let ((things
	 (mapcar 'butlast
		 (mapcar 'cdr
			 (apply 'append (mapcar 'second (even-nicer-functions)))))))
    (let ((one (consolidate (mapcar 'first things)))
	  (two (consolidate (mapcar 'second things))))
      (list one two))))

;;text -> json -> jonathan -> lisp objects/regular structs, struct-to-clos
;;All telegram types are lisp objects with regular naming schemes
;;all telegram functions take in what look like regular lisp objects and spit out
;;regular lisp objects
;;This solid foundation with provide a good platform for telegram

;;functions to start out with:
;;
;;getUpdates
;;sendMessage
;;deleteMessage
;;
;;json/jonathan <-> lisp objects, primarily used when 'serializing' and 'unserializing' for the
;;telegram web API
;;lisp objects with constructors
;;use alists rather than plists for json objects in telegram?
