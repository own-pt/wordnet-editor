;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(defpackage #:wordnet
  (:use #:cl :db.agraph :prolog :db.agraph.sparql
	:net.uri :net.aserve :net.html.generator
	:net.aserve.client))
