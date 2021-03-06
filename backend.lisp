(in-package :wordnet)

(defparameter *own-pt-source* !source:own-pt.nt)

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
  (let ((synset-1 (db.agraph::coerce-to-upi synset))
        (a-form-1 (db.agraph::coerce-to-upi (literal a-form :language "pt"))))
    (unless (and synset-1 a-form)
      (error "Unable to convert arguments to UPIs"))
    (select0-distinct ?ws
      (q- ?w  !wn30:lexicalForm (?? a-form-1))
      (q- ?ws !wn30:word ?w)
      (q- (?? synset-1) !wn30:containsWordSense ?ws))))

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

(defun make-unique-iri (basename ns n)
  (let ((uri (resource 
              (if (> n 0)
                  (format nil "~a-~a" basename n)
                  (format nil "~a" basename)) ns)))
    (if (or (get-triples-list :s uri) (get-triples-list :o uri))
        (make-unique-iri basename ns (1+ n))
        uri)))

(defun make-unique-word-iri (a-form ns)
  (make-unique-iri (format nil "word-~a" (clean-up-word a-form)) ns 0))

(defun add-word (a-form &key (ns "wn30pt"))
  (let* ((lit (literal a-form :language "pt"))
	 (words (select0-distinct ?w 
		  (q- ?w !rdf:type !wn30:Word)
		  (q- ?w !wn30:lexicalForm (?? lit)))))
    (or (car words)
	(let ((res (make-unique-word-iri a-form ns)))
	  (add-triple res !rdf:type !wn30:Word :g *own-pt-source*) 
	  (add-triple res !wn30:lexicalForm lit :g *own-pt-source*)
	  res))))

(defun define-synset (synset prop values)
  (assert (synset? synset))
  (delete-triples :s synset :p prop)
  (if (listp values)
      (dolist (v values) 
	(add-triple synset prop (literal v :language "pt") :g *own-pt-source*))
      (add-triple synset prop (literal values :language "pt") :g *own-pt-source*)))

(defun add-synset-prop (synset prop values)
  (assert (synset? synset))
  (if (listp values)
      (dolist (v values)
        (add-synset-prop1 synset prop v))
      (add-synset-prop1 synset prop values)))

(defun add-synset-prop1 (synset prop value)
  (assert (synset? synset))
  (unless (get-triples-list :s synset :p prop :o (literal value :language "pt"))
    (add-triple synset prop (literal value :language "pt") :g *own-pt-source*)))

(defun add-synset-word (synset a-form)
  (assert (synset? synset))
  (let* ((lit (literal a-form :language "pt"))
	 (a-word (add-word a-form)))
    (unless (synset-has-word? synset a-form)
      (with-blank-nodes (sense)
	(add-triple synset !wn30:containsWordSense sense :g *own-pt-source*)
	(add-triple sense !rdf:type !wn30:WordSense :g *own-pt-source*)
	(add-triple sense !rdfs:label lit :g *own-pt-source*)
	(add-triple sense !wn30:word a-word :g *own-pt-source*)))))

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
    (add-triple node !nomlex:noun a-noun :g *own-pt-source*)
    (add-triple node !nomlex:verb a-verb :g *own-pt-source*)
    (add-triple node !rdf:type !nomlex:Nominalization :g *own-pt-source*)
    (if prov
	(add-triple node !dc:provenance (literal prov) :g *own-pt-source*))
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


