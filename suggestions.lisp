(in-package :wordnet)

(defvar *suggestions-core* (make-instance 'solr:solr :uri "http://localhost:8983/solr/suggestions"))

(defun search-suggestions ()
  (solr:solr-result->doc-alist
   (solr:solr-query *suggestions-core*
		    :query "status:committed" 
		    :param-alist '((:rows . 1000000)))))

(defun process-suggestions ()
  (dolist (s (search-suggestions))
    (let* ((action (cdr (assoc :action s)))
	   (status (cdr (assoc :status s)))
	   (doc-id (cdr (assoc :doc_id s)))
	   (doc-type (cdr (assoc :doc_type s)))
	   (params (cdr (assoc :params s)))
	   (res (resource (format nil "synset-~a" doc-id) "wn30pt")))
      (when (and (string-equal "committed" status)
		 (string-equal "synset" doc-type))
	(cond ((string-equal "add-word-pt" action)
	       (format t "add [~a] to [~a]~%" params res)
	       (add-synset-word res params))
	      ((string-equal "remove-word-pt" action)
	       (format t "rm  [~a] to [~a]~%" params res)
	       (remove-word params :synset res)))))))
	    

