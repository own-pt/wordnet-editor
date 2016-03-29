
(in-package :wordnet)

(defun group-by (alist n &optional res)
  (if (null alist)
      (reverse res)
      (group-by (subseq alist n) n (cons (subseq alist 0 n) res))))

(defun split-gloss (gloss)
  (let* ((re "[ ]*;[ ]*\"[^\"]+\"")
	 (pos (cl-ppcre:all-matches re gloss))
	 (bw '(#\; #\" #\Space)))
    (if pos
	;; For each two consecutive numbers in the sequence, group
	;; start-end block. Each block is an example. The beginning,
	;; between position 0 and the first number, is the
	;; definition. Before return the parts with values, trim them
	;; removing leading space and \;
	(values (string-trim bw (subseq gloss 0 (car pos)))
		(loop for pair in (group-by pos 2)
		      collect (string-trim bw (subseq gloss (car pair) (cadr pair)))))
	(values gloss nil))))

(defmacro lit (value)
  `(literal ,value :language "en"))

(defun ingest-data (synset def examples)
  (add-triple synset !wn30:gloss (lit def))
  (dolist (e examples)
    (add-triple synset !wn30:example (lit e))))

(defun main ()
  (dolist (tr (select (?x ?g)
		(q- ?x !skos:inScheme !<http://wordnet.princeton.edu/>)
		(q- ?x !wn30:gloss ?g)))
    (multiple-value-bind (def examples)
	(split-gloss (part->string (cadr tr)))
      (if (and def examples)
	  (ingest-data (car tr) def examples)))))

;; should we remove old triples? maybe it can be done later with a
;; simples query! If a synset (EN) has more than one gloss, we can
;; remove the smaller one.
