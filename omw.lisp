;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker
;;
;; Function to export the openWordnet-PT data to the input format of
;; http://www.casta-net.jp/~kuribayashi/multi/

(in-package :wordnet)

(defun generate-omw (filename)
  (let ((words (select0 (?ss ?lex)
		   (q- ?word !wn30:lexicalForm ?lex)
		   (q- ?ws !wn30:word ?word)
		   (q- ?ss !wn30:containsWordSense ?ws))))
      (with-open-file (out filename :if-exists :supersede :direction :output)
	(dolist (w words)
	  (format out "~a~Tlemma~T~a~%" 
		  (cl-ppcre:regex-replace "wn30br:synset-" (part->string (car w) :format :concise) "") 
		  (part->string (cadr w)))))))
