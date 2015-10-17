
(in-package :wordnet)

;; working with words

(defun get-words (synset &key (ns "wn30pt")) 
  (let ((result (run-query-as-list "words-blank.sparql")))
    (dolist (w result)
      (let ((uri (resource (format nil "word-~a" (replace-regexp (cadr result) "[ ]+" "_")) ns)))
	(merge-nodes (car result) uri)))))


(defun clean-orphan-words ()
  (let ((result (mapcar #'car (run-query-as-list "words-orphan.sparql")))
	(res nil))
    (dolist (w result res)
      (push (delete-triples :s w) res))))
