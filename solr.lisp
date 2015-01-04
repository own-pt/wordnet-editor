
(in-package :wordnet)

(defun synset-to-alist (addr &key (suffix "en") (data nil) (plan nil))
  (labels ((tostr (apart regex replacement)
	     (multiple-value-bind (val type extra)
		 (upi->value apart)
	       (declare (ignore val extra))
	       (if (equal 0 type)
		   (cl-ppcre:regex-replace regex (part->string apart :format :concise) replacement)
		   (cl-ppcre:regex-replace regex (part->value apart) replacement))))
	   (adj-name (name)
	     (cond 
	       ((cl-ppcre:scan "word" name) 
		(concatenate 'string "word_" suffix)) 
	       ((cl-ppcre:scan "gloss" name)
		(concatenate 'string "gloss_" suffix))
	       (t name))))
    (let* ((synset (resource (concatenate 'string "synset-" addr) 
			     (concatenate 'string "wn30" suffix)))
	   (words (mapcar (lambda (w) (part->value (car w))) 
			  (sparql:run-sparql plan 
					     :with-variables `((?synset . ,synset))
					     :engine :sparql-1.1 :results-format :lists)))
	   (query (get-triples :s synset)))
      (do ((res (or data (list (cons :id addr))))
	   (a-triple (cursor-next-row query) (cursor-next-row query)))
	  ((null a-triple)
	   (if words (push (cons (intern (adj-name "word") :keyword) words) res) res))
	(cond
	  ((part= (predicate a-triple) !owl:sameAs)
	   (setf res (synset-to-alist addr :suffix "br" :data res :plan plan)))
	  ((not (part= (predicate a-triple) !wn30:containsWordSense)) 
	   (let ((key (intern (adj-name (tostr (predicate a-triple) ":" "_")) 
			      :keyword))
		 (val (tostr (object a-triple) "^wn30(en|br):synset-" "")))
	     (push (cons key val) res))))))))


(defun nomlex-to-alist (addr)
  (labels ((tostr (apart regex replacement)
	     (multiple-value-bind (val type extra)
		 (upi->value apart)
	       (declare (ignore val extra))
	       (if (equal 0 type)
		   (cl-ppcre:regex-replace regex (part->string apart :format :concise) replacement)
		   (cl-ppcre:regex-replace regex (part->value apart) replacement)))))
    (let ((query (get-triples :s addr)))
      (do ((res (list (cons :id (part->string addr :format :concise))))
	   (a-triple (cursor-next-row query)
		     (cursor-next-row query)))
	  ((null a-triple)
	   res)
	(let ((key (intern (tostr (predicate a-triple) ":" "_") :keyword)))
	  (if (member (predicate a-triple) '(!nomlex:noun !nomlex:verb) :test #'part=)
	      (let ((val (object (get-triple :s (object a-triple) :p !wn30:lexicalForm))))
		(push (cons key (part->value val)) res))
	      (push (cons key (tostr (object a-triple) "^nomlex:" "")) res)))))))


(defun load-nomlex-solr (blocksize)
  (let* ((current 0) 
	 (total 0)
	 (block-tmp nil)
	 (a-triple (get-triples :p !rdf:type :o !nomlex:Nominalization))) 
    (dolist (p objs)
      (let ((id (car a-triple)))
	(format *debug-io* "Processing ~a [~a/~a ~a]~%" id current blocksize total)
	(push (remove-duplicates (nomlex-to-alist id) :test #'equal) block-tmp)
	(setf current (1+ current))
	(if (> current blocksize)
	    (progn
	      (solr:solr-add* *solr* block-tmp :commit t)
	      (setf total (+ total current)
		    current 0
		    block-tmp nil)))))
    (solr:solr-add* *solr* block-tmp :commit t)))


(defun load-synsets-solr (blocksize)
  (let* ((current 0) 
	 (total 0)
	 (block-tmp nil)
	 (plan-words (sparql:parse-sparql (query-string "synset-words.sparql")))
	 (synsets (sparql:run-sparql (sparql:parse-sparql (query-string "all-synsets.sparql")) 
				     :results-format :lists))) 
    (dolist (p synsets)
      (let ((id (cl-ppcre:regex-replace "^wn30en:synset-" (part->string (car p) :format :concise) "")))
	(format *debug-io* "Processing ~a [~a/~a ~a]~%" id current blocksize total)
	(push (remove-duplicates (synset-to-alist id :plan plan-words) :test #'equal) block-tmp)
	(setf current (1+ current))
	(if (> current blocksize)
	    (progn
	      (solr:solr-add* *solr* block-tmp :commit t)
	      (setf total (+ total current)
		    current 0
		    block-tmp nil)))))
    (solr:solr-add* *solr* block-tmp :commit t)))
