;; (C) 2013 IBM Corporation
;;
;;  Author: Alexandre Rademaker
;;
;; For info why the dependencies file is necessary, read
;; http://weitz.de/packages.html

(asdf:defsystem #:wordnet
  :serial t
  :depends-on (:agclient :cl-ppcre :fare-csv :solr :alexandria :chillax :yason)
  :components ((:file "dependencies") 
	       (:file "packages"      :depends-on ("dependencies"))
	       (:file "ag-init"       :depends-on ("packages"))
	       (:file "utils"         :depends-on ("packages"))
	       (:file "omw"           :depends-on ("utils"))
	       (:file "backend"       :depends-on ("utils"))
	       (:file "solr"          :depends-on ("utils"))
	       (:file "couchdb"       :depends-on ("solr"))
	       (:file "deduplication" :depends-on ("utils"))
	       (:file "server"        :depends-on ("backend"))
	       (:file "nomlex-rdf"    :depends-on ("backend"))))
