 (defpackage :cl-telegram-bot
  (:use #:cl))
(in-package :cl-telegram-bot)

;;utils
(defun make-keyword (text)
  (cl-arrows:-> text
		(cl-strings:replace-all "_" "-")
		(nstring-upcase)
		(alexandria:make-keyword)))

;;;;Bot
(defclass bot ()
  ((lock
    :documentation "Limit access to the bot"
    :initform (bordeaux-threads:make-recursive-lock "bot lock")
    :accessor bot-lock)
   (id
    :documentation "Update id"
    :initform 0
    :accessor get-last-update-id)
   (token
    :initarg :token
    :documentation "Bot token given by BotFather"
    :accessor token
    :initform nil)
   (api-uri
    :initarg  :api-uri
    :initform "https://api.telegram.org/"
    :accessor api-uri)
   (endpoint
    :initarg :endpoint
    :reader get-endpoint
    :documentation "HTTPS endpoint")
   (file-endpoint
    :initarg :file-endpoint
    :accessor file-endpoint
    :documentation "HTTPS file-endpoint"
    :initform nil)))

(defmacro with-locked-bot ((bot) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((bot-lock ,bot))
     ,@body))
(defmethod print-object ((bot bot) stream)
  (print-unreadable-object
      (bot stream :type t)
    (format stream
            "id=~A" (get-last-update-id bot))))

(defmacro defbot (name)
  `(progn
     (defclass ,name (bot)
       ())

     (defun ,(alexandria:symbolicate 'make- name) (token)
       (make-instance ',name
                      :token token))))


(defmethod initialize-instance :after ((bot bot) &key &allow-other-keys)
  (with-accessors ((token         token)
                   (file-endpoint file-endpoint)
                   (api-uri       api-uri)) bot
    (setf (slot-value bot 'endpoint)
          (concatenate 'string api-uri "bot" token "/")
          (slot-value bot 'file-endpoint)
          (concatenate 'string api-uri "file/" "bot" token "/"))))

;;;Network
(progn
  (define-condition request-error (error)
    ((what :initarg :what :reader what))
    (:report (lambda (condition stream)
	       (format stream "Request error: ~A" (what condition)))))

  (defun obfuscate (url)
    (cl-ppcre:regex-replace "/bot.*?/"
			    url
			    "/bot<token>/"))

  (defun make-request (bot name json-string &key (streamp nil) timeout)
    "Perform HTTP request to 'name API method with 'options JSON-encoded object."
    (let ((url (concatenate 'string (get-endpoint bot) name)))
      (log:debug "Posting data to"
		 (obfuscate url)
		 json-string)
      (let* ((response (dexador:post url
				     :stream streamp
				     :headers '(("Content-Type" . "application/json"))
				     :content json-string
				     :timeout timeout))
	     (data (jonathan:parse response :as :jsown)))
	(unless (jsown:val data "ok")
	  (log:error "Wrong data received from the server" data)
	  (error 'request-error :what data))

	(jsown:val data "result")))))

;;;file
#+nil ;;FIXME ::what is going on here?
(defun download-file (b file-id)
  "Get the  path for a  file from a  file-id (see: get-file)  and then
   download it.  Returns nil if the value of the http response code is
   not  success (200);  otherwise it  will returns  three values:  the
   data, the http headers and the exension of the original file"
  (with-package :cl-telegram-bot
    (let* ((file-spec (decode (get-file b file-id))))
      (macrolet ((with-ok-results ((unserialized results) &body body)
		   `(let ((,results (slot-value ,unserialized (find-json-symbol :result))))
		      (if (slot-value ,unserialized (find-json-symbol :ok))
			  (progn ,@body)
			  nil))))
	(with-ok-results (file-spec results)
	  (alexandria:when-let* ((path      (access results 'file--path))
				 (uri       (concatenate 'string (file-endpoint b) path))
				 (extension (cl-ppcre:scan-to-strings "\\..*$" path)))
	    (dexador:get uri)))))))

;;;update
(defun get-updates (bot &key limit timeout)
  "https://core.telegram.org/bots/api#getupdates"
  (let* ((current-id (get-last-update-id bot))
         (results (make-request bot "getUpdates"
				(jonathan:to-json
				 (jsown:new-js
				   (:|offset| current-id)
				   (:|limit| limit)
				   (:|timeout| timeout))
				 :from :jsown)
                                :streamp t
                                :timeout timeout)))    
    (let ((updates results))
      (when updates
        (let ((max-id (reduce #'max
                              updates
                              :key
			      (lambda (x)
				(jsown:val x "update_id")))))
          ;; In original cl-telegram-bot a bug was here, because
          ;; it saved update's id only the first time, and after that,
          ;; just incremented that value
          (log:debug "Setting new" max-id)
          (setf (get-last-update-id bot)
                (+ max-id 1))))
    
      (values updates))))
