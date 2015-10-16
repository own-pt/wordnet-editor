(in-package :wordnet)

;; these were analyzed by Livy and Valeria and found to be invalid.
;; Keeping them here since we may want to regenerate our
;; morphosemantic links conversion from english to portugues again in
;; the future as we add and remove words from our PT synsets.
(defparameter *invalid-pt-morphosemantic-triples*
'((!wn30pt:wordsense-00924873-v-1 !nomlex:agent !wn30pt:wordsense-09762101-n-1)
  (!wn30pt:wordsense-01346003-v-1 !nomlex:agent !wn30pt:wordsense-10737431-n-1)
  (!wn30pt:wordsense-00416399-v-1 !nomlex:agent !wn30pt:wordsense-10071332-n-1)
  (!wn30pt:wordsense-00818805-v-1 !nomlex:agent !wn30pt:wordsense-09954355-n-1)
  (!wn30pt:wordsense-02220461-v-1 !nomlex:agent !wn30pt:wordsense-01107932-n-1)
  (!wn30pt:wordsense-02541251-v-1 !nomlex:agent !wn30pt:wordsense-08186047-n-1)
  (!wn30pt:wordsense-00413704-v-1 !nomlex:agent !wn30pt:wordsense-08413834-n-1)
  (!wn30pt:wordsense-01645601-v-1 !nomlex:agent !wn30pt:wordsense-00007347-n-1)
  (!wn30pt:wordsense-01970348-v-1 !nomlex:event !wn30pt:wordsense-07445480-n-1)
  (!wn30pt:wordsense-00270005-v-1 !nomlex:event !wn30pt:wordsense-07312221-n-1)
  (!wn30pt:wordsense-00703875-v-1 !nomlex:event !wn30pt:wordsense-05785508-n-2)
  (!wn30pt:wordsense-00121046-v-1 !nomlex:event !wn30pt:wordsense-05984584-n-1)
  (!wn30pt:wordsense-02237782-v-1 !nomlex:event !wn30pt:wordsense-01089778-n-2)
  (!wn30pt:wordsense-00298067-v-1 !nomlex:event !wn30pt:wordsense-00199707-n-1)
  (!wn30pt:wordsense-00910973-v-1 !nomlex:event !wn30pt:wordsense-07209965-n-2)
  (!wn30pt:wordsense-01171183-v-2 !nomlex:event !wn30pt:wordsense-00748515-n-1)
  (!wn30pt:wordsense-02079933-v-1 !nomlex:event !wn30pt:wordsense-06251781-n-1)
  (!wn30pt:wordsense-02208903-v-2 !nomlex:event !wn30pt:wordsense-13295657-n-2)
  (!wn30pt:wordsense-00158503-v-2 !nomlex:event !wn30pt:wordsense-05110185-n-1)
  (!wn30pt:wordsense-00772967-v-1 !nomlex:event !wn30pt:wordsense-06891022-n-1)
  (!wn30pt:wordsense-00550117-v-1 !nomlex:event !wn30pt:wordsense-00196084-n-1)
  (!wn30pt:wordsense-00029630-v-1 !nomlex:event !wn30pt:wordsense-07378059-n-1)
  (!wn30pt:wordsense-00351963-v-1 !nomlex:event !wn30pt:wordsense-15267536-n-2)
  (!wn30pt:wordsense-01405044-v-1 !nomlex:event !wn30pt:wordsense-00043902-n-1) 
  (!wn30pt:wordsense-00118523-v-1 !nomlex:event !wn30pt:wordsense-15133621-n-1)
  (!wn30pt:wordsense-02641957-v-1 !nomlex:event !wn30pt:wordsense-15272029-n-2) 
  (!wn30pt:wordsense-02208537-v-2 !nomlex:event !wn30pt:wordsense-13295657-n-2) 
  (!wn30pt:wordsense-02460619-v-1 !nomlex:event !wn30pt:wordsense-13295657-n-2)
  (!wn30pt:wordsense-02460199-v-1 !nomlex:event !wn30pt:wordsense-15274863-n-2)
  (!wn30pt:wordsense-01950798-v-1 !nomlex:event !wn30pt:wordsense-00061290-n-2)
  (!wn30pt:wordsense-00917772-v-3 !nomlex:event !wn30pt:wordsense-05775407-n-1)
  (!wn30pt:wordsense-00820976-v-5 !nomlex:event !wn30pt:wordsense-06880249-n-2)
  (!wn30pt:wordsense-02609764-v-4 !nomlex:event !wn30pt:wordsense-15266911-n-5) 
  (!wn30pt:wordsense-02561332-v-4 !nomlex:event !wn30pt:wordsense-00631378-n-1) 
  (!wn30pt:wordsense-00351963-v-2 !nomlex:event !wn30pt:wordsense-15267536-n-1) 
  (!wn30pt:wordsense-02425913-v-2 !nomlex:event !wn30pt:wordsense-15267536-n-1)
  (!wn30pt:wordsense-02609764-v-3 !nomlex:event !wn30pt:wordsense-15266911-n-2)
  (!wn30pt:wordsense-02208903-v-1 !nomlex:event !wn30pt:wordsense-15274863-n-2)))

(defparameter *regular* '("or" "ura" "ção" "mento" "gem" "nte" "cia" "sia" "ada"))
(defparameter *regressive* '("e" "o" "a"))
(defparameter *irregular* '("l" "m" "r"))

(defun get-synset-words (query synset)
  (mapcar (lambda (w) (part->value (car w))) 
          (sparql:run-sparql 
           query 
           :with-variables `((?synset . ,synset))
           :engine :sparql-1.1 
           :results-format :lists)))

(defun analyze-suffix (word)
  (dolist (s *regular*)
    (when (alexandria:ends-with-subseq s word)
      (return-from analyze-suffix (values s "REGULAR"))))
  (dolist (s *regressive*)
    (when (alexandria:ends-with-subseq s word)
      (return-from analyze-suffix (values s "REGRESSIVE"))))
  (dolist (s *irregular*)
    (when (alexandria:ends-with-subseq s word)
      (return-from analyze-suffix (values s "IRREGULAR")))))

(defun delete-invalid-pt-morphosemantic-triples ()
  (dolist (tr *invalid-pt-morphosemantic-triples*)
    (delete-triples  :s (first tr) :p (second tr) :o (third tr))))

(defun generate-morphosemantic-links-noun-suffix-report ()
  (let ((rows (run-query-as-list "morphosemantic-suffixes.sparql"))
        (cnt 0))
    (with-open-file (out "/tmp/morphosemantic-links-suffixes.csv" :direction :output :if-exists :supersede)
      (format out "word1,rel,word2,synset1,synset2,lex. file1, lex. file2, suffix, class~%")
      (dolist (rr rows)
        (destructuring-bind 
              (w1 relation w2 synset-id1 synset-id2 lex-file1 lex-file2) (mapcar #'upi->value rr)
          (multiple-value-bind (suffix class) (analyze-suffix w2)
            (format out "~a,~a,~a,~a-v,~a-n,~a,~a,~a,~a~%" w1 relation w2 synset-id1 synset-id2 lex-file1 lex-file2 suffix class)))))))

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
