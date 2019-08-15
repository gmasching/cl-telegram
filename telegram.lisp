;;(ql:quickload :cl-telegram-bot)
;;(ql:quickload :utility)

;;;Architecture of the cl-telegram bot

;;;start a spin loop in a subthread which requests updates from Telegram,
;;;and dispatches on those updates.
(defpackage #:the-bot
  (:use :cl :cl-telegram-util))
(in-package #:the-bot)

;;blindly coding
;;planned features:
;;persistence
;;multiple users
;;two-way communication. the bot can alert the person, the person can alert the bot

;;Initialize the lparallel library
(setf lparallel:*kernel* (lparallel:make-kernel 2))
;;The lparallel channel for asynchronously getting updates
(defparameter *async-updates-channel* nil)

;;Initialize the device which runs functions at a constant rate in real-time
(defparameter *ticker* nil)

(defparameter *current-time* 0)
(defun what-time (&optional (time (local-time:now)))
  (local-time:timestamp-to-unix time))

(defun start2 (&optional (token cl-telegram-bot::*token*))
  (subthread
   (lambda ()
     (start token))))

(defparameter *stop* t
  "Set this to t to stop the program.")
(defun stop ()
  (setf *stop* t))

(defparameter *update-queue* nil)
(defun get-pending-updates ()
  "Return the updates in the update queue as a list. The oldest updates 
come first in the queue, and also first in the list."
  (let ((update-objects nil))
    (loop :named out :do
       (multiple-value-bind (value existsp)
	   (lparallel.queue:try-pop-queue *update-queue*)
	 (if existsp
	     (push value update-objects)
	     (return-from out))))
    (values (nreverse update-objects))))

(defparameter *bot* nil)
(defun start (&optional (token cl-telegram-bot::*token*))
  (setf *stop* nil)
  (let ((*bot* (cl-telegram-bot::make-bot :token token))
	(*async-updates-channel* (lparallel:make-channel))
	(*ticker*
	 ;;A ticker that ticks every 1 second. the second number does not correspond to seconds?
	 (fps-independent-timestep:make-ticker 1 most-positive-fixnum 10))
	(*update-queue* (lparallel.queue:make-queue)))
    ;;FIXME::this is here to empty lost time. Bug?
    (fps-independent-timestep::tick *ticker* ((what-time)))

    ;;Dummy task to start polling
    (lparallel:submit-task *async-updates-channel*
			   (lambda () (make-task-return-type :initialize-dummy)))
    (loop
       (designate-polling-and-handle-updates *bot* *async-updates-channel* *ticker*)
       (when *stop* (return)))))
(defun designate-polling-and-handle-updates
    (&optional (bot *bot*) (channel *async-updates-channel*) (ticker *ticker*))
  (setf *current-time* (what-time))
  ;;Run 'one-process-iteration' at a fixed rate.
  (fps-independent-timestep::tick ticker (*current-time*)
    (tick bot (get-pending-updates)))
  (multiple-value-bind (value existsp) (lparallel:try-receive-result channel)
    (when (and existsp value)
      (with-task-return-type (type value) value
	(ecase type
	  ;;A get-updates task finished. Handle the recieved tasks, and
	  ;;start polling for the next round
	  (:get-updates
	   (dolist (update-object value) 
	     (lparallel.queue:push-queue update-object *update-queue*))
	   (submit-get-updates-task))
	  ;;A dummy task is used to initialize the system.
	  (:initialize-dummy
	   (submit-get-updates-task)))))))

(defun submit-get-updates-task (&optional (bot *bot*) (channel *async-updates-channel*))
  "Create a task to asynchronously wait for updates from telegram, apart from the
  main bot thread."
  (lparallel:submit-task
   channel
   (lambda ()
     (make-task-return-type
      :get-updates
      (mapcar 'create-update
	      (cl-telegram-bot::get-updates bot
					    :timeout 10))))))


(struct-to-clos:struct->class
 (defstruct update
   update_id
   type
   thing
   raw-data
   user))
(defmethod print-object ((update update) stream)
  (format stream
   "~%User:~a ~%Update ID:~a ~%Type:~a ~%Thing:~a"
   (update-user update)
   (update-update_id update)
   (update-type update)
   (update-thing update)))

(defun create-update (update-data)
  (let ((type nil)
	(thing nil))
    (block out
      ;;A telegram update has one of these types.
      (dolist (item '("message"
		      "edited_message"
		      "channel_post"
		      "edited_channel_post"
		      "inline_query"
		      "chosen_inline_result"
		      "callback_query"
		      "shipping_query"
		      "pre_checkout_query"))
	(multiple-value-bind (value existsp)
	    (get-json-object-member update-data item)
	  (when existsp
	    (setf type item
		  thing value)
	    (return-from out)))))
    (let ((user (get-json-object-members* thing '("from" "id")))
	  (update_id (get-json-object-member update-data "update_id")))
      (make-update :update_id update_id
		   :type type
		   :thing thing
		   :raw-data update-data
		   :user user))))

;;;Above is general code, below is test code

(defparameter *testcase*
  #+nil
  (:OBJ
   ("inline_keyboard"
    ((:OBJ ("text" . "A") ("callback_data" . "A1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1")))
    ((:OBJ ("text" . "Annnjlkjlkjlkjj;;;;;;;;;;;;;k") ("callback_data" . "A1")))))

 
  (jonathan:parse
   (alexandria:read-file-into-string
    (merge-pathnames
     "test/json.json"
     (asdf:system-source-directory :cl-telegram)))
   :as :jsown))

(defparameter *user-id-whitelist* ())
(progn
  (defun whitelist-user (user)
    (pushnew user *user-id-whitelist* :test 'eql))
  (defun ban-user (user)
    (setf *user-id-whitelist*
	  (remove user *user-id-whitelist*)))
  (defun user-whitelisted-p (user)
    (member user *user-id-whitelist* :test 'eql )))
(defun throw-out-bad-updates (updates)
  (remove-if-not (lambda (update)
		   (user-whitelisted-p (update-user update)))
		   updates))

(struct-to-clos:struct->class
 (defstruct ))


(defparameter *live-chats* (make-hash-table))
(defparameter *chat-id* nil)
(defparameter *ticks* 0)
(defun tick (bot update-objects)
  (incf *ticks*)
  (format t ".")
  ;;Remove updates that are not from a whitelisted user
  (let ((whitelisted-updates (throw-out-bad-updates update-objects)))
    (let ((whitelisted-updates-count (length whitelisted-updates))
	  (update-objects-count (length update-objects)))
      (unless (zerop update-objects-count)
	(format t "~%----Handling Updates----")
	(format t "~%Valid   :~a ~%Recieved:~a"
		whitelisted-updates-count
		update-objects-count)))
    (setf update-objects whitelisted-updates))
  (dolist (update-obj update-objects)
    (print update-obj)
    (print (get-json-object-member (update-thing update-obj) "text"))
    (let ((update (update-raw-data update-obj)))
      (multiple-value-bind (chatid existsp)
	  (get-json-object-members* update '("message" "chat"))
	;;chatid is the id of the chat the update is from
	;;FIXME::doees this belong here? only adding to live chats once the
	;;user texts the bot first?
	(when existsp
	  ;;add it to the live chats
	  (setf (gethash chatid *live-chats*) t)
	  ;;FIXME::chat deletion code... move somewhere else?
	  #+nil
	  (cl-telegram-bot/bindings::delete-message
	   bot
	   (get-json-object-members* update '("message" "chat" "id"))
	   (get-json-object-members* update '("message" "message_id"))
	   )))))
  
  (cl-telegram-bot::with-locked-bot (bot)
    (while ((let ((thing (next-thing-to-do-time)))
	      (and (not (eq thing :end))
		   (>= *current-time* thing))))
      (destructuring-bind (id todo) (todo-item-value (pop *todo-timeline*))
	(print id)
	(typecase todo
	  (string
	   (send-message todo :chat-id id)))))
    (when (< 1 (length *live-chats*))
      (error "what the hell? why are there more than one chat?"))

    (dolist (chat *live-chats*)
      (let ((*chat-id* (get-json-object-member chat "id")))
	#+nil
	(send-message *ticks*)
	#+nil
	(when (zerop (mod *ticks* 8))
	  (send-message "https://gamepedia.cursecdn.com/minecraft_gamepedia/c/c8/Wolf.png"))
	#+nil
	(when (zerop (mod *ticks* 4))
	  (send-message
	   (with-output-to-string (stream)
	     (print (utility::etouq (random 234)) stream))
	   :reply-markup
	   *testcase*))
	#+nil
	(add-task (list *chat-id* "google.com")
		  (what-time
		   (local-time:timestamp+ (local-time:now)
					  (random 100) :sec)))))))

(defun send-message (thing &rest rest &key &allow-other-keys)
  (apply 'cl-telegram-bot/bindings::send-message
	 (or (getf rest :bot) *bot*)
	 (or (getf rest :chat-id) *chat-id*)
	 (format nil "~a" thing) rest))


;;;whitelisted users
;;;- input whitelist and bot key
;;;throw out updates not on the whitelist
;;;repl
;;;inline keyboard system
;;;remember chat history, also sort by time
;;;persistent? 
;;;separation of polling and sending

;;FIXME::O(N) because it uses a list, gets really slow, quadratically?
;;This is an implementation of a simple scheduler. Tasks are scheduled at
;;some point in the future. The variable which holds these tasks is the
;;variable *todo-timeline*.
(progn
  (defparameter *todo-timeline* (list
				 (cons "end" :end)))
  (defun add-task (task time)
    (let ((tail
	   (member-if (lambda (a-time)
			(if (eq a-time :end)
			    t
			    (> a-time time)))
		      *todo-timeline* :key 'cdr)))
      (destructive-insert-first-in-list
       (cons task time)
       tail))
    *todo-timeline*)

  (defun destructive-insert-first-in-list (item list)
    (if (consp list)
	(let ((old-car (car list)))
	  (setf (car list) item
		(cdr list) (cons old-car (cdr list)))
	  list)
	(error "not a list ~a" list))))

(defun next-thing-to-do-time ()
  (cdr (first *todo-timeline*)))

(defun todo-item-value (todo-item)
  (car todo-item))
