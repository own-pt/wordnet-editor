;;; these are various functions that were used to clean up the own-pt triplestore
;;; that aren't needed anymore; they are retained here if we need to do similar cleanups
;;; in the future.
(in-package :wordnet)


;;; this procedure goes through all the nodes under the default graph
;;; and attemps to move them to the proper graphs essentially all
;;; subjects with certain prefixes that belong to the default graph
;;; will be moved, along all its related nodes, to the proper graph
;;; per the following table:

;;http://logics.emap.fgv.br/wn/ own-pt.nt
;;https://w3id.org/own-pt/nomlex/ nomlex.nt
;;https://w3id.org/own-pt/wn30-en/ wordnet-en.nt
;;https://w3id.org/own-pt/wn30-pt/ own-pt.nt
;;http://wordnet.princeton.edu/ wordnet-en.nt

;; after FIX-DEFAULT-GRAPH is execute, there will remain around 3000 blank nodes
;; these can be fixed with FIX-REMANING-BLANK-NODES-IN-DEFAULT-GRAPH

(defun move-to-graph (node graph)
  (dolist (tr (get-triples-list :g (default-graph-upi *db*) :s node))
    (add-triple node (predicate tr) (object tr) :g graph)
    (delete-triple (triple-id tr)))
  (dolist (tr (get-triples-list :g (default-graph-upi *db*) :o node))
    (add-triple (subject tr) (predicate tr) node :g graph)
    (delete-triple (triple-id tr))))
  
(defun fix-default-graph ()
  (dolist (tr (get-triples-list :g (default-graph-upi *db*)))
    (let* ((s (subject tr))
           (subject-value (upi->value (subject tr))))
      (when (stringp subject-value)
        (when (alexandria:starts-with-subseq "http://logics.emap.fgv.br/wn/" subject-value)
          (move-to-graph s !source:own-pt.nt))
        (when (alexandria:starts-with-subseq "https://w3id.org/own-pt/nomlex/" subject-value)
          (move-to-graph s !source:own-pt.nt))
        (when (alexandria:starts-with-subseq "https://w3id.org/own-pt/wn30-pt/" subject-value)
          (move-to-graph s !source:own-pt.nt))
        (when (alexandria:starts-with-subseq "https://w3id.org/own-pt/wn30-en/" subject-value)
          (move-to-graph s !source:wordnet-en.nt))
        (when (alexandria:starts-with-subseq "http://wordnet.princeton.edu/" subject-value)
          (move-to-graph s !source:wordnet-en.nt))))))
  
(defun fix-remaining-blank-nodes-in-default-graph ()
  (dolist (tr (get-triples-list :g (default-graph-upi *db*)))
    (when (blank-node-p (subject tr))
      (move-to-graph (subject tr) !source:own-pt.nt))))

(defun clean-up-word (str)
  (cl-ppcre:regex-replace-all "[^\\w]" str "_"))

(defun get-suffix (type)
  (cond ((part= type !wn30:VerbSynset)
         "v")
        ((part= type !wn30:AdjectiveSatelliteSynset)
         "a")
        ((part= type !wn30:AdjectiveSynset)
         "a")
        ((part= type !wn30:NounSynset)
         "n")
        ((part= type !wn30:AdverbSynset)
         "r")))

(defun make-wordsense-id (synset prefix n)
  (labels ((synset-type (synset)
	     (object (get-triple :s synset :p !rdf:type)))
	   (synset-id (synset)
	     (upi->value (object (get-triple :s synset :p !wn30:synsetId))))
	   (type-suffix (type)
             (get-suffix (synset-type synset)))
	   (encode-synset (s)
	       (format nil "~a-~a"
		       (synset-id synset)
		       (type-suffix (synset-type synset)))))
    (upi (resource 
	  (format nil "~a-~a-~a" prefix (encode-synset synset) n)
	  "wn30pt"))))

(defun process-wordsense (synset blank prefix n)
  (let ((ws (make-wordsense-id synset prefix n)))
    ;; if the newly generated wordsense is alread present on the
    ;; data-store, move to the next one
    (if (or (get-triples-list :s ws) (get-triples-list :o ws))
        (process-wordsense synset blank prefix (1+ n))
        (merge-nodes blank ws))))

(defun process-wordsenses (synset objects prefix)
  (when objects
    (progn
      (process-wordsense synset (car objects) prefix 1)
      (process-wordsenses synset (cdr objects) prefix))))

(defun get-wordsenses (synset)
  (mapcar #'object (get-triples-list :s synset :p !wn30:containsWordSense)))

(defun process-all-blank-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-with-blank-nodes.sparql"))))
    (dolist (s table)
      (let* ((wordsenses (get-wordsenses s))
             (blank-wordsenses (remove-if-not #'blank-node-p wordsenses)))
        (process-wordsenses s blank-wordsenses "wordsense")))))

(defun process-word (old lf ns n)
  (let ((uri (resource 
              (if (> n 0)
                  (format nil "word-~a-~a" lf n) 
                  (format nil "word-~a" lf)) ns)))
    (if (or (get-triples-list :s uri) (get-triples-list :o uri))
        (process-word old lf ns (1+ n))
        (merge-nodes old uri))))

(defun process-all-blank-pt-words (&key (ns "wn30pt"))
  (let ((result (run-query-as-list "pt-blank-words.sparql")))
    (dolist (w result)
      (process-word (car w) (clean-up-word (upi->value (cadr w))) ns 0))))

(defun process-all-blank-en-words (&key (ns "wn30en"))
  (let ((result (run-query-as-list "en-blank-words.sparql")))
    (dolist (w result)
      (process-word (car w) (clean-up-word (upi->value (cadr w))) ns 0))))

;;this hack was necessary because we had invalid URIs in the database
;;specifically a couple of words under http://arademaker.github.com/wn30-br/instances/
;;had spaces in them.  since none of the libraries that I tried support URIs with
;;spaces in them I had to resort to using SUBSEQ.
(defun process-all-words-with-invalid-chars (&key (ns "wn30pt"))
  (dolist (w (get-triples-list :p !rdf:type :o !old-wn30:Word))
    (when (not (blank-node-p (subject w)))
      (let ((word (upi->value (subject w))))
	(when (find #\space word)
	  (let ((uri (resource (format nil "word-~a"
				       (clean-up-word (subseq word 52))) ns)))
	    (merge-nodes (subject w) uri)))))))

;;;
;;; fixes the predicates synsetId and tagCount.  They need to make
;;; sure that the literals are properly typed as
;;; xsd:nonnegativeInteger
;;;
;;; https://w3id.org/own-pt/wn30/schema/tagCount
;;; https://w3id.org/own-pt/wn30/schema/synsetId
;;;
;;; xs:nonNegativeInteger

(defun fix-incorrectly-typed-literals1 (predicate)
  (dolist (tr (get-triples-list :p predicate))
    (let* ((s (subject tr))
           (p (predicate tr))
           (o (object tr))
           (g (graph tr))
           (tl (literal (upi->value o) :datatype !xs:nonNegativeInteger)))
      (delete-triple (triple-id tr))
      (add-triple s p tl :g g))))

(defun fix-incorrectly-typed-literals ()
  (fix-incorrectly-typed-literals1 !wn30:tagCount)
  (fix-incorrectly-typed-literals1 !wn30:synsetId))

