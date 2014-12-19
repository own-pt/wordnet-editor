
(in-package :wordnet)

(defparameter *nomlex-types* '(("lex"    . !nomlex:LexicalNominalization) 
			       ("agent"  . !nomlex:AgentNominalization)
			       ("result" . !nomlex:ResultNominalization)
			       ("event"  . !nomlex:EventNominalization)))


(defun read-nomlex-registers (filename)
  (labels ((get-elem (n reg)
	     (cl-ppcre:regex-replace-all " " (nth n reg) "_")))
    (let ((regs (fare-csv:read-csv-file filename))) 
      (dolist (r (cdr regs))
	(let ((node (resource (format nil "nomlex-~a-~a" (get-elem 0 r) 
				      (get-elem 1 r)) "nomlex-br"))
	      (a-verb (resource (format nil "word-~a" (get-elem 0 r)) "wn30br"))
	      (a-noun (resource (format nil "word-~a" (get-elem 1 r)) "wn30br")))
	  (add-triple node !nomlex:verb a-verb)
	  (add-triple node !nomlex:noun a-noun)
	  (add-word a-verb (nth 0 r))
	  (add-word a-noun (nth 1 r))
	  (add-triple node !rdf:type !nomlex:Nominalization)
	  (if (not (string= (nth 2 r) "NA")) 
	      (add-triple node !nomlex:plural (literal (nth 2 r) :language "pt")))
	  (dolist (type (cl-ppcre:split ";" (nth 4 r)))
	    (if (and (not (string= (nth 4 r) "NA")) 
		     (assoc type *nomlex-types* :test #'string=))
		(add-triple node !rdf:type (cdr (assoc type *nomlex-types* :test #'string=)))))
	  (if (not (string= (nth 3 r) "NA"))
	      (let ((prov (car (cl-ppcre:split ":" (nth 3 r)))))
		(add-triple node !dc:provenance (literal prov :language "pt")))))))))


(defun add-translation (lemma1 lemma2)
  (let ((pair (select0 (?w1 ?w2)
		(q- ?w1 !wn30:lexicalForm (?? (literal lemma1 :language "pt")))
		(q- ?w2 !wn30:lexicalForm (?? (literal lemma2))))))
    (if (car pair)
	(add-triple (caar pair) !wn30:hasTranslation (cadar pair)))))


(defun get-nomlex (part noun &key (prop !nomlex:verb))
  (caar (sparql:run-sparql (sparql:parse-sparql (query-string "nominalization.sparql"))
			   :with-variables `((?lv . ,(literal part :language "pt"))
					     (?ln . ,(literal noun :language "pt"))
					     (?pr . ,prop))
			   :engine :sparql-1.1
			   :results-format :lists)))


(defun add-nomlex (part noun &key (prop !nomlex:verb) (prov "AnCora"))
  (let ((node (resource (format nil "nomlex-~a-~a" part noun) "nomlex-br"))
	(a-part (add-word part))
	(a-noun (add-word noun)))
    (add-triple node prop a-part)
    (add-triple node !nomlex:noun a-noun)
    (add-triple node !rdf:type !nomlex:Nominalization)
    (add-triple node !dc:provenance (literal prov))
    node))


(defun proc-line (line)
  (let ((noun (car line))
	(verb (cadr line))) 
    (cond ((or (equal noun "NO VERB")
	       (equal noun "")
	       (equal verb "NO VERB")
	       (equal verb "")) 
	   (append line (list 'none)))
	  ((get-nomlex verb noun)
	   (append line (list 'already-included)))
	  (t (append line (list (add-nomlex verb noun)))))))

