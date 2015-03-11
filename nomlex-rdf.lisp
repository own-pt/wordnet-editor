
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


(defun get-nomlex (part noun &key (prop !nomlex:verb))
  (caar (sparql:run-sparql (sparql:parse-sparql (query-string "nominalization.sparql"))
			   :with-variables `((?lv . ,(literal part :language "pt"))
					     (?ln . ,(literal noun :language "pt"))
					     (?pr . ,prop))
			   :engine :sparql-1.1
			   :results-format :lists)))




