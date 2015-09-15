
(in-package :wordnet)

(defparameter *ptrs* (list !wn30:hypernymOf 
			   !wn30:hyponymOf  
			   !wn30:instanceOf 
			   !wn30:hasInstance
			   !wn30:memberMeronymOf 
			   !wn30:memberHolonymOf
			   !wn30:similarTo
			   !wn30:entails
			   !wn30:substanceMeronymOf 
			   !wn30:substanceHolonymOf
			   !wn30:partMeronymOf 
			   !wn30:partHolonymOf
			   !wn30:classifiedByTopic
			   !wn30:classifiedByRegion
			   !wn30:classifiedByUsage
			   !wn30:classifiesByTopic
			   !wn30:classifiesByRegion
			   !wn30:classifiesByUsage
			   !wn30:derivationallyRelated
			   !wn30:causes
			   !wn30:sameVerbGroupAs
			   !wn30:attribute
			   !wn30:antonymOf
			   !wn30:seeAlso
			   !wn30:participleOf
			   !wn30:adjectivePertainsTo 
			   !wn30:adverbPertainsTo))


(defun synset? (synset)
  (get-triples-list :s synset :p !wn30:synsetId))

(defun synset-has-word? (synset a-form)
  (select0-distinct ?ws
    (q- ?w  !wn30:lexicalForm (?? (literal a-form :language "pt")))
    (q- ?ws !wn30:word ?w)
    (q- (?? synset) !wn30:containsWordSense ?ws)))

(defun remove-word (a-form &key (synset nil) (debug nil))
  (let ((vars `((?lf . ,(literal a-form :language "pt"))))
	(marked nil))
    (labels ((mark (a-list)
	       (dolist (a-item a-list)
		 (push a-item marked))))
      (if synset
	  (push `(?ss . ,synset) vars))
      (let ((table (run-sparql (parse-sparql (query-string "word-ws-ss.sparql")
					     (alexandria:alist-hash-table (collect-namespaces)))
			       :with-variables vars
			       :engine :sparql-1.1 :results-format :lists)))
	(dolist (line table)
	  (mark (get-triples-list :s (third line) :p !wn30:containsWordSense :o (second line)))
	  (mark (get-triples-list :s (second line)))
	  (unless synset 
	    (mark (get-triples-list :s (first line)))))
	(dolist (m marked marked)
	  (if debug 
	      (format *debug-io* "Del ~s~%" m))
	  (delete-triple (triple-id m)))))))

(defun add-word (a-form &key (ns "wn30pt"))
  (let* ((lit (literal a-form :language "pt"))
	 (words (select0-distinct ?w 
		  (q- ?w !rdf:type !wn30:Word)
		  (q- ?w !wn30:lexicalForm (?? lit)))))
    (or (car words)
	(let* ((uri (format nil "word-~a" (replace-regexp a-form "[ ]+" "_")))
	       (res (resource uri ns)))
	  (add-triple res !rdf:type !wn30:Word) 
	  (add-triple res !wn30:lexicalForm lit)
	  res))))

(defun define-synset (synset prop values)
  (assert (synset? synset))
  (delete-triples :s synset :p prop)
  (if (listp values)
      (dolist (v values) 
	(add-triple synset prop (literal v :language "pt")))
      (add-triple synset prop (literal values :language "pt"))))

(defun add-synset-prop (synset prop values)
  (assert (synset? synset))
  (if (listp values)
      (dolist (v values) 
	(add-triple synset prop (literal v :language "pt")))
      (add-triple synset prop (literal values :language "pt"))))

(defun add-synset-word (synset a-form)
  (assert (synset? synset))
  (let* ((lit (literal a-form :language "pt"))
	 (a-word (add-word a-form)))
    (unless (synset-has-word? synset a-form)
      (with-blank-nodes (sense)
	(add-triple synset !wn30:containsWordSense sense)
	(add-triple sense !rdf:type !wn30:WordSense)
	(add-triple sense !rdfs:label lit)
	(add-triple sense !wn30:word a-word)))))

(defun get-synsets (a-string)
  (select0-distinct ?ss 
    (generating ?tr (let ((cursor (freetext-get-triples (?? a-string) :index "lf")))
		      (lambda () (cursor-next-row cursor))))
    (lisp ?sub (subject ?tr))
    ; (lisp ?obj (object ?tr))
    (q- ?sense !wn30:word ?sub)
    (q- ?ss !wn30:containsWordSense ?sense)))

(defun describe-synset-0 (synset)
  (let* ((gloss (if (get-triples-list :s synset :p !wn30:gloss) 
		    (object (car (get-triples-list :s synset :p !wn30:gloss)))))
	 (rels (sparql:run-sparql (sparql:parse-sparql (query-string "synset-rels.sparql"))
	 			  :with-variables `((?synset . ,synset))
	 			  :results-format :lists))
	 (lexfl (object (car (get-triples-list :s synset :p !wn30:lexicographerFile))))
	 (same  (object (car (get-triples-list :s synset :p !owl:sameAs))))
	 (words (sparql:run-sparql (sparql:parse-sparql (query-string "synset-words.sparql"))
	 			  :with-variables `((?synset . ,synset))
	 			  :results-format :lists))
	 (words (select ?lf
		  (q- (?? synset) !wn30:containsWordSense ?sense)
		  (q- ?sense !wn30:word ?word)
		  (q- ?word !wn30:lexicalForm ?lf))))
    (list gloss lexfl same words rels)))

(defun describe-synset (synset)
  (let (data words rels)
    ;; if there are not too many matches for the synset, it may be faster to
    ;; make one get-triples call rather than 4 get-triple calls
    ;;
    ;; upi= is more efficient than part= so we make sure properties is a list of
    ;; UPIs, not future-parts
    ;;
    ;; We do both the single-valued and the multiple-valued properties here.
    ;; Note that it is better to iterate over the cursor and select what we want
    ;; rather than getting a list of everything and removing what we don't want.
    (let ((properties (mapcar #'upi '(!wn30:gloss !wn30:lexicographerFile !owl:sameAs
                                      !wn30:synsetId))))
      (iterate-cursor (triple (get-triples :s synset))
        (let ((p (predicate triple)))
          ;; make sure *ptrs* contains UPIs and not future-parts
          (when (member p *ptrs* :test 'upi=)
            (push (object triple) rels))
          ;; single value properties
          (when (member p properties :test 'upi=)
            (push (object triple) data)))))

    ;; Prolog does depth-first search; SPARQL does breadth-first.
    ;; breadth-first is often significantly faster (though it also often
    ;; uses more memory
    (let ((cursor (sparql:run-sparql (query-string "synset-words.sparql") 
				     :results-format :cursor
				     :with-variables `((?synset . synset)))))
      (iterate-cursor (lf cursor)
		      (push (copy-upi (first lf)) words)))
    (list words rels data)))


(defun add-nomlex (noun verb &key (prov nil))
  (assert (and noun verb))
  (let ((node (resource (format nil "nomlex-~a-~a" verb noun) "nm-pt"))
	(a-noun (add-word noun))
	(a-verb (add-word verb)))
    (add-triple node !nomlex:noun a-noun)
    (add-triple node !nomlex:verb a-verb)
    (add-triple node !rdf:type !nomlex:Nominalization)
    (if prov
	(add-triple node !dc:provenance (literal prov)))
    node))

(defun remove-nomlex (&key (noun nil) (verb nil))
  (assert (or noun verb))
  (let ((vars nil))
    (if verb (push `(?verb . ,(literal verb :language "pt")) vars))
    (if noun (push `(?noun . ,(literal noun :language "pt")) vars))
    (let ((resp (run-sparql (parse-sparql (query-string "nominalization.sparql")
					  (alexandria:alist-hash-table (collect-namespaces)))
			    :with-variables vars
			    :engine :sparql-1.1 :results-format :lists)))
      (dolist (r resp)
	(delete-triples :s (car r))))))

(defmacro nomlex (cmd &body body)
  (let (verb noun)
    (dolist (b body)
      (cond
	((equal 'noun (car b))
	 (setf noun (cadr b)))
	((equal 'verb (car b))
	 (setf verb (cadr b)))
	(t (error "I don't know this clause"))))
    (cond
      ((equal 'add cmd)
       `(add-nomlex ,noun ,verb))
      ((equal 'remove cmd)
       `(remove-nomlex :noun ,noun :verb ,verb))
      (t (error "I don't know this command")))))

(defun make-wordsense-id (synset prefix n)
  (labels ((synset-type (synset)
	     (object (get-triple :s synset :p !rdf:type)))
	   (synset-id (synset)
	     (upi->value (object (get-triple :s synset :p !wn30:synsetId))))
	   (type-suffix (type)
	     (upi->value (object (get-triple :s type :p !wn30:suffixCode))))
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

;;; Initially, OpenWordnet-PT was in an inconsitent state where some synsets
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

(defun process-all-blank-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-with-blank-nodes.sparql"))))
    (dolist (s table)
      (process-wordsenses s (get-wordsenses s) "tmp-wordsense" 1))))

(defun rename-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-without-blank-nodes.sparql"))))
    (dolist (s table)
      (process-wordsenses s (get-wordsenses s) "wordsense" 1))))


;; working with words

(defun get-words (synset &key (ns "wn30pt")) 
  (let ((result (run-query-as-list "words-blank.sparql")))
    (dolist (w result)
      (let ((uri (resource (format nil "word-~a" (replace-regexp (cadr result) "[ ]+" "_")) ns)))
	(merge-nodes (car result) uri)))))
