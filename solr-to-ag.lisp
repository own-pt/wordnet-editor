
(in-package :solr-to-ag)


(defvar *suggestions* (make-instance 'solr:solr :uri "http://localhost:8983/solr/suggestions"))
(defvar *votes*       (make-instance 'solr:solr :uri "http://localhost:8983/solr/votes"))

(defun get-solr-docs (query solr-repository &key (rows 10) (start 0))
  (let ((response (solr:solr-query solr-repository
				   :query query
				   :param-alist `(("rows" . ,rows) 
						  ("start" . ,start)))))
    (multiple-value-bind (a b c)
	(solr:solr-result->response-count response)
      (values (solr:solr-result->doc-alist response) (list a b c)))))


(defun at (s p o)
  (add-triple s p (typecase o
		    (string (literal o))
		    (number (literal (write-to-string o)))
		    (t o))
	      :g !<https://w3id.org/own-pt/app/>))

(defun res (string-fmt field doc &key (ns "app-i"))
  (resource (format nil string-fmt (get-value field doc)) ns))


(defun get-value (field doc)
  (cdr (assoc field doc)))


(defun add-vote (doc)
  (let ((vote  (res "vote-~a" :id doc)))
    (at vote !rdf:type             !app-s:Vote)
    (at vote !prov:wasAttributedTo (get-value :user doc))
    (at vote !prov:generatedAtTime (get-value :date doc))
    (at vote !app-s:value          (get-value :value doc))
    (at vote !app-s:subject        (res "suggestion-~a" :suggestion_id doc))))


(defun add-suggestion (doc)
  (assert (and (equal (get-value :doc_type doc) "synset")
	       (equal (get-value :type doc) "suggestion")))
  (let ((suggestion  (res "suggestion-~a" :id doc))
	(action (format nil "~{~a~^-~}"
			(subseq (cl-ppcre:split "-" (get-value :action doc)) 0 2))))
    (at suggestion !rdf:type !app-s:Suggestion)
    (at suggestion !app-s:action         action)
    (at suggestion !app-s:param          (get-value :params doc))
    (at suggestion !app-s:status         (get-value :status doc))
    (at suggestion !app-s:voteSum        (get-value :sum_votes doc))
    (at suggestion !app-s:voteScore      (get-value :vote_score doc))
    (at suggestion !prov:generatedAtTime (get-value :date doc))    
    (at suggestion !app-s:subject        (res "synset-~a" :doc_id doc :ns "wn30pt"))
    (at suggestion !app-s:prov           (get-value :provenance doc))
    (at suggestion !prov:wasAttributedTo (or (get-value :user doc) "unknown"))))


;; to run open the triple store and execute both commands

;; (open-triple-store ...)
;; (mapcar #'add-suggestion (get-solr-docs "type:suggestion" *suggestions* :rows 126000))
;; (mapcar #'add-vote (get-solr-docs "*:*" *votes* :rows 8300))
