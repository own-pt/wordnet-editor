(ql:quickload :solr)
(ql:quickload :cl-json)
(load "agraph.fasl")
 
(defpackage :my-app
  (:use :common-lisp :solr :cl-json :db.agraph.user))

(in-package :my-app)

(db.agraph.user::enable-!-reader)
(db.agraph.user::enable-print-decoded t) 
(db.agraph.user::create-triple-store "version1"  
  :triple-store-class 'db.agraph.user::remote-triple-store  
  :server "logics.emap.fgv.br"  
  ;  :catalog "my-catalog" ;; uncomment and use an existing catalog name  
                           ;; if desired  
  :port 10035 :user "rafael" :password "rafael2016")

(db.agraph.user::register-namespace "prov" "http://www.w3.org/ns/prov#"  
   :errorp nil) 
(db.agraph.user::register-namespace "wn30" "https://w3id.org/own-pt/wn30/schema/"  
   :errorp nil) 
(db.agraph.user::register-namespace "wn30pt" "https://w3id.org/own-pt/wn30-pt/instances/"  
   :errorp nil) 
(db.agraph.user::register-namespace "rdfs" "http://www.w3.org/2000/01/rdf-schema#"  
   :errorp nil) 
(db.agraph.user::register-namespace "own-pt-api-s" "http://example.org/"  
   :errorp nil) 
(db.agraph.user::register-namespace "own-pt-api-v" "http://example2.org/"  
   :errorp nil) 

(defvar *solr-suggestions* (make-instance 'solr :uri "http://localhost:8983/solr/suggestions"))
(defvar *solr-votes* (make-instance 'solr :uri "http://localhost:8983/solr/votes"))

(defun get-data (&key (rows 2) (start 0) (solr-repository *solr-suggestions*))
    (with-input-from-string
	(s 
	 (solr-query solr-repository 
		     :param-alist `(("wt" . "json") 
				    ("rows" . ,rows) 
				    ("start" . ,start))))
      (json:decode-json s)))

(defun get-response (&key (rows 2) (start 0) (solr-repository *solr-suggestions*))
  (assoc ':RESPONSE (get-data :rows rows :start start :solr-repository solr-repository)))

(defun get-docs-list (&key (rows 2) (start 0) (solr-repository *solr-suggestions*))
  (cdadddr (get-response :rows rows :start start :solr-repository solr-repository)))

(defun populate-triples (&key (rows 2)
			      (start 0) 
			      (solr-repository *solr-suggestions*)
			      (output-function #'generate-suggestion))
  (loop for i in (get-docs-list :rows rows :start start :solr-repository solr-repository) do
	(funcall output-function i)))

(defun generate-vote (doc)
  (let*((vote  (db.agraph.user::literal 
		      (concatenate 'string "vote" (cdr (assoc ':ID doc)))))
	(type !prov:Entity)
	(user (db.agraph.user::literal
	       (cdr (assoc ':USER doc))))
	(date (db.agraph.user::literal 
	       (write-to-string (cdr (assoc ':DATE doc)))))
	(value (db.agraph.user::literal 
		(write-to-string (cdr (assoc ':VALUE doc)))))
	(subject (db.agraph.user::literal 
		  (concatenate 'string "suggestion"
		    (cdr (assoc ':SUGGESTION--ID doc))))))
    (db.agraph:add-triple vote !rdf:type type)
    (db.agraph.user::add-triple vote !prov:wasAttributedTo user)
    (db.agraph.user::add-triple vote !prov:generatedAtTime date)
    (db.agraph.user::add-triple vote !own-pt-api-v:value value)
    (db.agraph.user::add-triple vote !own-pt-api-v:subject subject)))
  

(defun generate-suggestion (doc)
  (let*((suggestion  (db.agraph.user::literal 
		      (concatenate 'string "suggestion" (cdr (assoc ':ID doc)))))
	(type !prov:Entity)
	(action (db.agraph.user::literal 
		 (cdr (assoc ':ACTION doc))))
	(user (db.agraph.user::literal
	       (if (null (assoc ':USxER doc))
		   "Lost"
		 (cdr (assoc ':USER doc)))))
	(date (db.agraph.user::literal 
	       (write-to-string (cdr (assoc ':DATE doc)))))
	(value (db.agraph.user::literal 
		(cdr (assoc ':PARAMS doc))))
	(subject (db.agraph.user::resource 
		  (concatenate 'string
		    "https://w3id.org/own-pt/wn30/schema/" "synset-"
		    (cdr (assoc ':DOC--ID doc)))))
	(sum-votes (db.agraph.user::literal 
		    (write-to-string (cdr (assoc ':SUM--VOTES doc)))))
	(vote-score (db.agraph.user::literal 
		     (write-to-string (cdr (assoc ':VOTE--SCORE doc))))))
    (db.agraph.user::add-triple suggestion !rdf:type type)
    (db.agraph.user::add-triple suggestion !prov:wasAttributedTo user)
    (db.agraph.user::add-triple suggestion !prov:generatedAtTime date)
    (db.agraph.user::add-triple suggestion !own-pt-api-v:value value)
    (db.agraph.user::add-triple suggestion !own-pt-api-v:action action)
    (db.agraph.user::add-triple suggestion !own-pt-api-v:subject subject)
    (db.agraph.user::add-triple suggestion !own-pt-api-v:sumVotes sum-votes)
    (db.agraph.user::add-triple suggestion !own-pt-api-v:voteScore vote-score)))
    
    
    
(defun main ()
  (labels ((loop-documents (size &key (step 1000)
				      (start 0)
				      (solr-repository *solr-suggestions*)
				      (output-function #'generate-suggestion))
	     (if (>= step size)
		 (progn
		   (populate-triples :rows size 
				     :start start 
				     :solr-repository solr-repository
				     :output-function output-function)
		   (db.agraph.user::commit-triple-store))
	       (progn
		  (populate-triples :rows step :start start   :solr-repository solr-repository
				    :output-function output-function)
		  (db.agraph.user::commit-triple-store)
		  (loop-documents (- size step) :start (+ start step)
							  :solr-repository solr-repository
				   :output-function output-function)))))
    (let*((number-suggestions 
	   (cdr (assoc ':NUM-FOUND (cdr (get-response)))))
	  (number-votes
	   (cdr (assoc ':NUM-FOUND
		       (cdr (get-response 
			     :solr-repository *solr-votes*))))))
      (loop-documents number-suggestions)
      (loop-documents number-votes :solr-repository *solr-votes* 
		      :output-function #'generate-vote))))



 
;; (add-triple !ex:Mammal !rdf:type !owl:Class)     
    
(defun get-suggestions-2 (&key (rows 2))
  (json:with-decoder-simple-clos-semantics
      (let ((json:*json-symbols-package* nil))
	(let ((x (json:decode-json-from-string
		  (solr-query *solr-suggestions* :param-alist `(("wt" . "json") ("rows" . ,rows))))))
	  x))))

(defun get-response-2 (&key (rows 2))
  (with-slots (RESPONSE) (get-suggestions-2 :rows rows) (values RESPONSE)))

(defun get-docs-2 (&key (rows 2))
  (with-slots (DOCS) (get-response-2 :rows rows) (values DOCS)))

