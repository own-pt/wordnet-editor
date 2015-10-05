(ql:quickload :fare-csv)
(ql:quickload :split-sequence)
(ql:quickload :agclient)

(defpackage :morpho
  (:use :cl :fare-csv :db.agraph :db.agraph.sparql :split-sequence))

(in-package :morpho)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "source" "file://")
(register-namespace "nomlex" "https://w3id.org/own-pt/nomlex/schema/")
(register-namespace "wn30"   "https://w3id.org/own-pt/wn30/schema/" :errorp nil)

(defparameter *morphosemantic-graph* !source:morphosemantic-links.xlsx)
(defparameter *relation-mapping* nil)

(defun get-letter-type (ss-type)
  (cond ((equal "1" ss-type) "n")
        ((equal "2" ss-type) "v")
        ((equal "3" ss-type) "a")
        ((equal "4" ss-type) "r")
        ((equal "5" ss-type) "a")
        (t nil)))

(defun get-synset-id (typed-offset)
  (format nil "~a-~a" (get-letter-type (subseq typed-offset 0 1))
          (subseq typed-offset 1)))

;;  ("arg1 sensekey" "arg1 offset" "relation" "arg2 sensekey" "arg2 offset" "arg1 gloss (abbreviated)" "arg2 gloss (abbreviated)") 
(defun process-row (cols)
  (let* ((sense-key1 (first cols))
         (synset1 (get-synset-id (second cols)))
         (relation (third cols))
         (sense-key2 (fourth cols))
         (synset2 (get-synset-id (fifth cols)))
         (triples1 (get-triples-list :p !wn30:senseKey :o (literal sense-key1)))
         (triples2 (get-triples-list :p !wn30:senseKey :o (literal sense-key2))))
    (assert (= 1 (length triples1)))
    (assert (= 1 (length triples2)))
    (let ((word-sense1 (subject (first triples1)))
          (word-sense2 (subject (first triples2))))
      (format t "~a ~a ~a~%" word-sense1 relation word-sense2) 
      (add-triple word-sense1 
                  (cdr (assoc relation *relation-mapping* :test #'equal))
                  word-sense2
                  :g  *morphosemantic-graph*))))
           
(defun initialize-predicates ()
  (setf *relation-mapping* 
        (pairlis '("agent" "body-part" "by-means-of" "destination" "event"
                   "instrument" "location" "material" "property" "relation"
                   "result" "state" "undergoer" "uses" "vehicle")
                 '(!nomlex:agent !nomlex:bodyPart !nomlex:byMeansOf
                   !nomlex:destination !nomlex:event !nomlex:instrument
                   !nomlex:location !nomlex:material !nomlex:property
                   !nomlex:relation !nomlex:result !nomlex:state
                   !nomlex:undergoer !nomlex:uses !nomlex:vehicle))))

(defun run ()
  (initialize-predicates)
  (with-open-file (in "morphosemantic-links.csv")
    (mapc #'process-row (cdr (read-csv-stream in)))))

