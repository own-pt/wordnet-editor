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

;;;;;;;;;;;;;;

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
  (merge-nodes blank (make-wordsense-id synset prefix n)))

(defun process-wordsenses (synset objects prefix n)
  (when objects
    (progn
      (process-wordsense synset (car objects) prefix n)
      (process-wordsenses synset (cdr objects) prefix (1+ n)))))

;;; A quick word on these two functions:

;;; Initially, OpenWordnet-PT was in an inconsistent state where some synsets
;;; had "containsWordSense" relations with blank nodes AND regular nodes.
;;; To avoid any name conflict, we simply opted to rename ALL objects (regardless of
;;; whether they are blank or not) to "tmp-wordsense-<synset-id>-<number>" and then
;;; rename ALL these new nodes to its final value "wordsense-<synset-id>-<number>".
;;;
;;; So, given an inconsistent OpenWordnet-PT, the following methods need to be called
;;; in the correct order:
;;;
;;; (process-all-blank-wordsenses)
;;; 
;;; at this point all synsets will have "containsWordSense" pointing
;;; to "tmp-wordsense-xxx-yyy" nodes
;;;
;;; (rename-wordsenses)
;;;
;;; at this point all "tmp-wordsense-*" will be renamed to "wordsense-*".
;;;
;;; Since there was no information about the order of words in the
;;; first place, it doesn't matter for these methods the final order
;;; of the words.

 
(defun get-wordsenses (synset)
  (mapcar #'object (get-triples-list :s synset :p !wn30:containsWordSense)))

;;; warning: these are corrupting the triple-store (begin)
;;; refrain from using until the bug is fixed.
(defun process-all-blank-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-with-blank-nodes.sparql"))))
    (dolist (s table)
      (process-wordsenses s (get-wordsenses s) "tmp-wordsense" 1))))

(defun rename-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-without-blank-nodes.sparql"))))
    (dolist (s table)
      (process-wordsenses s (get-wordsenses s) "wordsense" 1))))

(defun process-all-blank-pt-words (&key (ns "wn30pt"))
  (let ((result (run-query-as-list "pt-blank-words.sparql")))
    (dolist (w result)
      (let ((uri (resource 
		  (format nil "word-~a"
			  (clean-up-word (upi->value (cadr w)))) ns)))
        (merge-nodes (car w) uri)))))

(defun process-all-blank-en-words (&key (ns "wn30en"))
  (let ((result (run-query-as-list "en-blank-words.sparql")))
    (dolist (w result)
      (let ((uri (resource 
		  (format nil "word-~a"
			  (clean-up-word (upi->value (cadr w)))) ns)))
        (merge-nodes (car w) uri)))))
;;; warning: these are corrupting the triple-store (end)

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

