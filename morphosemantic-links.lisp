(in-package :wordnet)

(defun get-synset-words (query synset)
  (mapcar (lambda (w) (part->value (car w))) 
          (sparql:run-sparql query 
                             :with-variables `((?synset . ,synset)) ;;(list (cons '?synset  synset))
                             :engine :sparql-1.1 
                             :results-format :lists)))


(defun generate-morphosemantic-links-pt-report ()
  (let ((rows (run-query-as-list "morphosemantic-links.sparql"))
        (cnt 0))
    (with-open-file (out "/tmp/morphosemantic-links-pt.html" :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morphosemantic links PT.</h1><table>")
      (dolist (rr rows)
        (destructuring-bind (w1 w2 relation synsetId1 synsetId2 gloss1 gloss2) rr
          (incf cnt)
          (format out "<ul>")
          (format out "<li>[~a] <b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b>~%"
                  cnt
                  (upi->value w1)
                  relation
                  (upi->value w2))
          (format out "<li><a href=\"http://wnpt.brlcloud.com/wn/search?term=~a-v\">verb</a> (~a)~%"
                  (upi->value synsetId1)
                  (upi->value gloss1))
          (format out "<li><a href=\"http://wnpt.brlcloud.com/wn/search?term=~a-n\">noun</a> (~a)~%"
                  (upi->value synsetId2)
                  (upi->value gloss2))
          (format out "</ul>"))))))

;; select distinct ?nm ?sptid1 ?sptid2 ?lf1 ?lf2 ?p ?g1 ?g2 ?lws1 ?lws2
(defun generate-missing-nouns-html-report ()
  (let ((rows (run-query-as-list "missing-nouns.sparql"))
        (synset-words-query (sparql:parse-sparql (query-string "synset-words.sparql"))))
    (with-open-file (out "/tmp/morpholinks-nouns.html" :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morpholinks sem tradu&ccedil;&atilde;o para o substantivo.</h1><table>")
      (dolist (rr rows)
        (destructuring-bind (nomlex synsetId1 synsetId2 ptSynset1 ptSynset2 nomlex-verb nomlex-noun relation gloss1 gloss2 en-word1 en-word2) rr
          (format out "<ul>~%")
          (format out "<li><b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b> | <b>~a</b> &rArr; ~a</li>" 
                  (upi->value en-word1)
                  relation
                  (upi->value en-word2)
                  (upi->value nomlex-verb)
                  (upi->value nomlex-noun))
          (format out "<li><a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-v\">~a-v</a> (~a)</li>" 
                  (upi->value synsetId1)
                  (upi->value synsetId1)
                  (upi->value gloss1))
          (let ((words (get-synset-words synset-words-query ptSynset1)))
            (when words 
              (format out "<ul>")
              (format out "<li>~{~a~^, ~}" words)
              (format out "</ul>")))
          (format out "<li><a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-n\">~a-n</a> (~a)</li>"
                  (upi->value synsetId2)
                  (upi->value synsetId2)
                  (upi->value gloss2))
          (let ((words (get-synset-words synset-words-query ptSynset2)))
            (when words 
              (format out "<ul>")
              (format out "<li>~{~a~^, ~}" words)
              (format out "</ul>")))
          (format out "</ul>~%")))
      (format out "</table></body></html>"))))

(defun generate-missing-verbs-html-report ()
  (let ((rows (run-query-as-list "missing-verbs.sparql"))
        (synset-words-query (sparql:parse-sparql (query-string "synset-words.sparql"))))
    (with-open-file (out "/tmp/morpholinks-verbs.html" :direction :output :if-exists :supersede)
      (format out "<html><header><META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"></header><body><h1>Morpholinks sem tradu&ccedil;&atilde;o para o verbo.</h1><table>")
      (dolist (rr rows)
        (destructuring-bind (nomlex synsetId1 synsetId2 ptSynset1 ptSynset2 nomlex-verb nomlex-noun relation gloss1 gloss2 en-word1 en-word2) rr
          (format out "<ul>~%")
          (format out "<li><b>~a</b> &rArr; <i>~a</i> &rArr; <b>~a</b> | ~a &rArr; <b>~a</b></li>" 
                  (upi->value en-word1)
                  relation
                  (upi->value en-word2)
                  (upi->value nomlex-verb)
                  (upi->value nomlex-noun))
          (format out "<li><a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-v\">~a-v</a> (~a)</li>" 
                  (upi->value synsetId1)
                  (upi->value synsetId1)
                  (upi->value gloss1))
          (let ((words (get-synset-words synset-words-query ptSynset1)))
            (when words 
              (format out "<ul>")
              (format out "<li>~{~a~^, ~}" words)
              (format out "</ul>")))
          (format out "<li><a target=\"_blank\" href=\"http://wnpt.brlcloud.com/wn/synset?id=~a-n\">~a-n</a> (~a)</li>"
                  (upi->value synsetId2)
                  (upi->value synsetId2)
                  (upi->value gloss2))
          (let ((words (get-synset-words synset-words-query ptSynset2)))
            (when words 
              (format out "<ul>")
              (format out "<li>~{~a~^, ~}" words)
              (format out "</ul>")))
          (format out "</ul>~%")))
      (format out "</table></body></html>"))))
