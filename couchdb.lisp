(in-package :wordnet)

;;; TODO: this is essentially the same as solr.lisp, but for CouchDB.
;;;       Both files can be unified.

(defun merge-duplicate-keys (alist)
  (let ((r (make-hash-table)))
    (dolist (pair alist)
      (let ((key (car pair))
            (val (cdr pair)))
        (multiple-value-bind (value ok) (gethash key r)
          (if ok
              (if (listp value)
                  (setf (gethash key r) (push val value))
                  (setf (gethash key r) (list val value)))
              (setf (gethash key r) val)))))
    (alexandria:hash-table-alist r)))

#|

Sample CouchDB chillax server creation:

(defparameter *server* (make-instance 'chillax:yason-server
				      :host *server-url*
				      :port 443
				      :securep t
				      :username *username*
				      :password *password*
				      :object-as-alist-p t
				      :parse-object-key-fun (lambda (string) (intern string *package*))))

(defparameter *cloudant-db* (chillax:ensure-db *server* "wn"))

|#

(defun load-nomlex-couchdb (blocksize db)
  (let* ((current 0) 
	 (total 0)
	 (block-tmp nil)
	 (query (get-triples :p !rdf:type :o !nomlex:Nominalization))) 
    (do* ((a-triple (cursor-next-row query)
		    (cursor-next-row query)))
	 ((null a-triple)
	  (chillax:bulk-post-documents db block-tmp))
      (format *debug-io* "Processing ~a [~a/~a ~a]~%" (part->string (subject a-triple)) current blocksize total)
      (push (merge-duplicate-keys 
	     (remove-duplicates 
	      (nomlex-to-alist (subject a-triple))
	      :test #'equal))
	    block-tmp)
      (setf current (1+ current))
      (if (> current blocksize)
	  (progn
	    (chillax:bulk-post-documents db block-tmp)
	    (setf total (+ total current)
		  current 0
		  block-tmp nil))))))

(defun load-synsets-couchdb (blocksize db)
  (let* ((current 0) 
	 (total 0)
	 (block-tmp nil)
	 (plan-words (sparql:parse-sparql (query-string "synset-words.sparql")))
	 (synsets (sparql:run-sparql (sparql:parse-sparql (query-string "all-synsets.sparql")) 
				     :results-format :lists))) 
    (dolist (p synsets)
      (let ((id (cl-ppcre:regex-replace "^wn30en:synset-"
					(part->string (car p) :format :concise) "")))
	(format *debug-io* "Processing ~a [~a/~a ~a]~%"
		id current blocksize total)
	(push (merge-duplicate-keys 
	       (remove-duplicates 
		(synset-to-alist id :plan plan-words)
		:test #'equal)) block-tmp)
	(setf current (1+ current))
	(if (> current blocksize)
	    (progn
	      (chillax:bulk-post-documents db block-tmp)
	      (setf total (+ total current)
		    current 0
		    block-tmp nil)))))
    (chillax:bulk-post-documents db block-tmp)))

