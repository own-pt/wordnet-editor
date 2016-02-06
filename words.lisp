
(in-package :wordnet)

;; working with words

(defun process-word (old lf ns n)
  (let ((uri (resource 
              (if (> n 0)
                  (format nil "word-~a-~a" lf n) 
                  (format nil "word-~a" lf)) ns)))
    (if (or (get-triples-list :s uri) (get-triples-list :o uri))
        (process-word old lf ns (1+ n))
        (merge-nodes old uri))))

(defun rename-all-blank-pt-words (&key (ns "wn30pt"))
  (let ((result (run-query-as-list "pt-blank-words.sparql")))
    (dolist (w result)
      (process-word (car w) (clean-up-word (upi->value (cadr w))) ns 0))))

(defun rename-all-blank-en-words (&key (ns "wn30en"))
  (let ((result (run-query-as-list "en-blank-words.sparql")))
    (dolist (w result)
      (process-word (car w) (clean-up-word (upi->value (cadr w))) ns 0))))

(defun rename-all-blank-words ()
  (rename-all-blank-en-words)
  (rename-all-blank-pt-words))

(defun clean-orphan-words ()
  (let ((result (mapcar #'car (run-query-as-list "words-orphan.sparql")))
	(res nil))
    (dolist (w result res)
      (push (delete-triples :s w) res))))
