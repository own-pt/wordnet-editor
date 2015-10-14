(in-package :wordnet)

(defparameter *to-delete* '(1 2 3))

(defun get-synset-words (query synset)
  (mapcar (lambda (w) (part->value (car w))) 
          (sparql:run-sparql 
           query 
           :with-variables `((?synset . ,synset))
           :engine :sparql-1.1 
           :results-format :lists)))

(defun generate-list-of-morphosemantic-links-to-delete ()
  (let ((rows (run-query-as-list "morphosemantic-links.sparql"))
        (cnt 0))
    (with-open-file (out "/tmp/morphosemantic-links-pt.txt" 
                         :direction :output :if-exists :supersede)
      (dolist (rr rows)
        (destructuring-bind (w1 w2 relation synsetId1 synsetId2 
                                gloss1 gloss2 s1 s2) rr
          (incf cnt)
          (when (member cnt *to-delete*)
            (format out "~a ~a ~a~%" s1 relation s2)))))))

(defun generate-morphosemantic-links-suffix-report ()
  (let ((rows (run-query-as-list "morphosemantic-suffixes.sparql"))
        (cnt 0))
    (with-open-file (out "/tmp/morphosemantic-links-words.csv" :direction :output :if-exists :supersede)
      (dolist (rr rows)
        (destructuring-bind (w1 relation w2) rr
          (let ((word1 (upi->value w1))
                 (word2 (upi->value w2)))
            (format out "~a, ~a, ~a~%" word1 relation word2)))))))

(defun generate-morphosemantic-links-pt-report ()
  (let ((rows (run-query-as-list "morphosemantic-links.sparql"))
        (cnt 0))
    (with-open-file (out "/tmp/morphosemantic-links-pt.html" :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morphosemantic links PT.</h1><table>")
      (dolist (rr rows)
        (destructuring-bind (w1 w2 relation 
                                synsetId1 synsetId2 gloss1 gloss2) rr
          (incf cnt)
          (format out "<ul>")
          (format out "<li>[~a] <b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b>~%"
                  cnt
                  (upi->value w1)
                  relation
                  (upi->value w2))
          (format out "<li><a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/search?term=~a-v\">verb</a> (~a)~%"
                  (upi->value synsetId1)
                  (upi->value gloss1))
          (format out "<li><a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/search?term=~a-n\">noun</a> (~a)~%"
                  (upi->value synsetId2)
                  (upi->value gloss2))
          (format out "</ul>"))))))

;; select distinct ?nm ?sptid1 ?sptid2 ?lf1 ?lf2 ?p ?g1 ?g2 ?lws1 ?lws2
(defun generate-missing-nouns-html-report ()
  (let ((rows (run-query-as-list "missing-nouns.sparql"))
        (synset-words-query (sparql:parse-sparql (query-string "synset-words.sparql"))))
    (with-open-file (out "/tmp/morpholinks-nouns.html" 
                         :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morpholinks sem tradu&ccedil;&atilde;o para o substantivo.</h1><table>")
      (format out "<ol>~%")
      (dolist (rr rows)
        (destructuring-bind (nomlex synsetId1 synsetId2 ptSynset1 ptSynset2 
                                    nomlex-verb nomlex-noun relation 
                                    gloss1 gloss2 en-word1 en-word2) rr
          (format out "<li style=\"width: 60%;\"><b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b> | <b>~a</b> &rArr; ~a<br>" 
                  (upi->value en-word1)
                  relation
                  (upi->value en-word2)
                  (upi->value nomlex-verb)
                  (upi->value nomlex-noun))
          (format out "<a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-v\">~a-v</a> (~a)<br>" 
                  (upi->value synsetId1)
                  (upi->value synsetId1)
                  (upi->value gloss1))
          (let ((words (get-synset-words synset-words-query ptSynset1)))
            (when words (format out "&rArr; ~{~a~^, ~}<br>" words)))

          (format out "<a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-n\">~a-n</a> (~a)<br>"
                  (upi->value synsetId2)
                  (upi->value synsetId2)
                  (upi->value gloss2))
          (let ((words (get-synset-words synset-words-query ptSynset2)))
            (when words (format out "&rArr; ~{~a~^, ~}" words)))
          (format out "</li>~%")))
      (format out "</ul></table></body></html>"))))

(defun check-heuristic (en-noun pt-noun)
  (and (or (alexandria:ends-with-subseq "er" en-noun)
           (alexandria:ends-with-subseq "or" en-noun))
       (or (alexandria:ends-with-subseq "or" pt-noun)
           (alexandria:ends-with-subseq  "nte" pt-noun))))

;; Palavras com -er e -or em EN terão a maioria dos seus
;; correspondentes em -or ou -nte em PT , logo você poderia fazer um
;; relatório apenas com estas sugestões. Cruzar palavras com -er/-or
;; em EN com palavras com -or ou -nte em PT.
(defun generate-missing-nouns-html-report-h1 ()
  (let ((rows (run-query-as-list "missing-nouns.sparql"))
        (synset-words-query (sparql:parse-sparql (query-string "synset-words.sparql"))))
    (with-open-file (out "/tmp/morpholinks-nouns-heuristic-1.html" :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morpholinks sem tradu&ccedil;&atilde;o para o substantivo (er/or - or/nte).</h1><table>")
      (format out "<ol>~%")
      (dolist (rr rows)
        (destructuring-bind (nomlex synsetId1 synsetId2 ptSynset1 ptSynset2
                                    nomlex-verb nomlex-noun relation
                                    gloss1 gloss2 en-word1 en-word2) rr
          (when (check-heuristic (upi->value en-word2) (upi->value nomlex-noun))
            (format out "<li style=\"width: 60%;\"><b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b> | <b>~a</b> &rArr; ~a<br>" 
                    (upi->value en-word1)
                    relation
                    (upi->value en-word2)
                    (upi->value nomlex-verb)
                    (upi->value nomlex-noun))
            (format out "<a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-v\">~a-v</a> (~a)<br>" 
                    (upi->value synsetId1)
                    (upi->value synsetId1)
                    (upi->value gloss1))
            (let ((words (get-synset-words synset-words-query ptSynset1)))
              (when words (format out "&rArr; ~{~a~^, ~}<br>" words)))
            
            (format out "<a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-n\">~a-n</a> (~a)<br>"
                    (upi->value synsetId2)
                    (upi->value synsetId2)
                    (upi->value gloss2))
            (let ((words (get-synset-words synset-words-query ptSynset2)))
              (when words (format out "&rArr; ~{~a~^, ~}" words)))
            (format out "</li>~%"))))
      (format out "</ol></table></body></html>"))))

(defun generate-missing-verbs-html-report ()
  (let ((rows (run-query-as-list "missing-verbs.sparql"))
        (synset-words-query
         (sparql:parse-sparql (query-string "synset-words.sparql"))))
    (with-open-file (out "/tmp/morpholinks-verbs.html" 
                         :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morpholinks sem tradu&ccedil;&atilde;o para o verbo.</h1><table>")
      (format out "<ol>~%")
      (dolist (rr rows)
        (destructuring-bind (nomlex synsetId1 synsetId2 ptSynset1 ptSynset2 
                                    nomlex-verb nomlex-noun relation
                                    gloss1 gloss2 en-word1 en-word2) rr
          (format out "<li style=\"width: 60%;\"><b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b> | ~a &rArr; <b>~a</b><br>" 
                  (upi->value en-word1)
                  relation
                  (upi->value en-word2)
                  (upi->value nomlex-verb)
                  (upi->value nomlex-noun))
          (format out "<a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-v\">~a-v</a> (~a)<br>" 
                  (upi->value synsetId1)
                  (upi->value synsetId1)
                  (upi->value gloss1))
          (let ((words (get-synset-words synset-words-query ptSynset1)))
            (when words (format out "&rArr; ~{~a~^, ~}<br>" words)))
          
          (format out "<a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-n\">~a-n</a> (~a)<br>"
                  (upi->value synsetId2)
                  (upi->value synsetId2)
                  (upi->value gloss2))
          (let ((words (get-synset-words synset-words-query ptSynset2)))
            (when words (format out "&rArr; ~{~a~^, ~}" words)))
          (format out "</li>~%")))
      (format out "</ol></table></body></html>"))))
