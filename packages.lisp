;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(defpackage #:wordnet
  (:use #:cl :excl
	:db.agraph :prolog :db.agraph.sparql :net.uri))

(defpackage #:solr-to-ag
  (:use #:cl :excl :db.agraph :net.uri))

