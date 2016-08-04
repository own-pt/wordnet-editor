(in-package :neo4j)

(defun concise (part)
  (cl-ppcre:regex-replace "wn30:" (part->concise part) ""))

(defun describe-resource (part)
  (let* ((triples (get-triples-list :s part :limit nil))
	 (values (mapcar (lambda (ts)
			   (list (predicate ts) (object ts)
				 (type-code->type-name (upi-type-code (object ts)))))
			 triples))
	 (parameters nil)
	 (types nil))
    (dolist (v values (values parameters types))
      (if (member (caddr v) '(:literal :literal-short :literal-typed :literal-language))
	  (push (list (concise (car v)) (part->value (cadr v))) parameters)
	  (if (part= !rdf:type (car v))
	      (push (concise (cadr v)) types))))))


(defun encode-synset (synset)
  (multiple-value-bind (props ll)
      (describe-resource synset)
    (yason:with-object ()
      (yason:encode-object-element "statement"
				   (format nil "CREATE (n:~{~a~^:~} {props}) RETURN n" ll))
      (yason:with-object-element ("parameters")
	(yason:with-object ()
	  (yason:with-object-element ("props")
	    (yason:with-object ()
	      (dolist (v props)
		(yason:encode-object-element (car v) (cadr v))))))))))

(defun encode-synsets (synsets &key (stream *standard-output*) (indent nil))
  (yason:with-output (stream :indent indent)
    (yason:with-object ()
      (yason:with-object-element ("statements")
	(yason:with-array ()
	  (dolist (synset synsets)
	    (encode-synset synset)))))))


;; mudar para versao interativa com recursao! (submit list) -> (submit
;; list bloco)
(defun submit-neo (synsets &key (bs 2000))
  (let ((drakma:*text-content-types* (cons (cons "application" "json")
					   drakma:*text-content-types*))
	(bloco nil))
    (labels ((submit (bloco)
	       (drakma:http-request "http://localhost:7474/db/data/transaction/commit"
				    :method :post
				    :content-type "application/json; charset=utf-8"
				    :accept "application/json; charset=UTF-8"
				    :content (lambda (stream)
					       (encode-synsets bloco :stream stream)))))
      (do* ((top synsets
		 (cdr synsets)))
	   ((null top)
	    (if bloco (submit bloco)))
	(push (car top) bloco)
	(if (> (length bloco) bs)
	    (progn
	      (submit bloco)
	      (setf bloco nil)))))))

;; given a initial set of nodes (pending) the ideia is to transverse
;; the graph collecting node-descriptions and relations. relations
;; should be add in the end only, using the same
;; block-size. block-size helps to make regular commits. At each
;; interaction, a node is removed from pending and new nodes
;; (adjacents) may be added in pending. the node removed from pending
;; must be 'marked' as visited, moved to visited, to avoid loops. the
;; initial set of nodes can be given as a query to guarante that all
;; nodes we are interested will be considered.
(defun graph-transverse (pending visited current-block relations block-size)
  "...")

;; pending error encoding: 
;; (let ((synsets (list !wn30pt:synset-11502497-n))
;;       (drakma:*header-stream* *standard-output*))
;;   (submit-neo synsets))

;; CREATE INDEX bastante limitado!

;; initial value for pending (above):
;; select distinct ?r
;; {
;;   VALUES ?type { wn30:Synset
;;                  wn30:AdverbSynset wn30:AdjectiveSatelliteSynset wn30:AdjectiveSynset 
;;                  wn30:NounSynset wn30:VerbSynset 
;;                  wn30:Word wn30:WordSense } 
;;   ?r a ?type .
;; }
