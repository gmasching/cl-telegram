(defsystem :cl-telegram
  :depends-on ("plump"
	       "utility"
	       "alexandria"
	       "kebab"
	       "cl-arrows"
	       "jonathan"
	       "dexador"
	       "log4cl"
	       "trivial-backtrace"
	       "cl-strings"
	       "fps-independent-timestep"
	       "jsown"
	       "cl-ppcre"
	       "lparallel"
	       "local-time"
	       "uncommon-lisp")
  :serial t
  :components
  ((:file "big")
   (:file "bindings")
   (:file "telegram")))
