
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


;; (defun get-synsets (a-string)
;;   (mapcar (lambda (tr)
;; 	    (select (?ss)
;; 	      (q- ?ss !wn30:containsWordSense ?sense)
;; 	      (q- ?sense !wn30:word (?? (subject tr)))))
;; 	  (collect-cursor (freetext-get-triples a-string :index "lf") :transform #'copy-triple)))

(defun get-synsets (a-string)
  (select0-distinct ?ss 
    (generating ?tr (let ((cursor (freetext-get-triples (?? a-string) :index "lf")))
		      (lambda () (cursor-next-row cursor))))
    (lisp ?sub (subject ?tr))
    ; (lisp ?obj (object ?tr))
    (q- ?sense !wn30:word ?sub)
    (q- ?ss !wn30:containsWordSense ?sense)))


(defun describe-synset (synset)
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
		  (q- ?word !wn30:lexicalForm ?lf)))
	 )
    (list gloss lexfl same words rels)))


(defun describe-synset-1 (synset)
  (let ((record (make-hash-table ))
	(types (get-triples-list :s :p !rdfs:type :limit nil))
	(id    (get-triple :s synset :p !wn30:synsetId))
	(same  (get-triple :s synset :p !owl:sameAs))
	(words (select ?lf 
		 (q- (?? synset) !wn30:containsWordSense ?sense)
		 (q- ?sense !wn30:word ?word)
		 (q- ?word !wn30:lexicalForm ?lf)))
	)
    (loop for p in (list '(!wn30:gloss :gloss) 
			 '(!wn30:lexicographerFile :lexfile))
	  for a-triple = (get-triple :s synset :p (car p))
	  do (if a-triple 
		 (setf (gethash (cadr p) record) (part->string (object a-triple)))))
    (rels (remove-if-not (lambda (atr) (member (predicate atr) *ptrs* :test #'part=))
			     (get-triples-list :s synset :limit nil)))

    (push words data)
    (push rels data)
    (reverse data)))


(defun describe-synset-2 (synset)
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



(defmacro with-synset (synset-id &body body)
  (let ((cmds nil))
    (dolist (clause body cmds)
      (cond 
	((equal (car clause) 'add)
	 (push `(add-synset-word res ,(cadr clause)) cmds))
	((equal (car clause) 'remove)
	 (push `(remove-word ,(cadr clause) :synset res :debug t) cmds))
	(t (error "I don't know this command ~a" (car clause)))))
    `(let ((res (resource (format nil "synset-~a" (quote ,synset-id)) "wn30pt")))
       ,@cmds)))


