
(asdf:defsystem #:suggestions
    :serial t
    :depends-on ("cl-ppcre" "cl-fad")
    :components ((:file "packages")
		 (:file "utils" :depends-on ("packages"))
		 (:file "suggestion-reader" :depends-on ("utils"))
		 (:file "statistics" :depends-on ("suggestion-reader"))))
		 
