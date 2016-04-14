;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(defpackage #:wordnet
  (:use #:cl :excl
	:db.agraph :prolog :db.agraph.sparql :net.uri)
  (:export
   #:file-string
   #:clean-up-word
   #:query-string
   #:run-query-as-list))

(defpackage #:solr-to-ag
  (:use #:cl :excl :db.agraph :net.uri))

(defpackage #:cstnews
  (:use #:cl :split-sequence :wordnet :db.agraph :db.agraph.sparql))


