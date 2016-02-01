
(in-package :wordnet)

(defun make-new-wordsense-id (synset)
  (labels ((new-part (synset n)
	     (resource (cl-ppcre:regex-replace "synset-([0-9]{8}-[anvr])"
					       (upi->value synset)
					       (format nil "wordsense-\\1-~a" n)))))
    (let ((senses (get-wordsenses synset)))
      (do* ((next 1 (1+ next))
	    (res (new-part synset next)
		 (new-part synset next)))
	   ((not (member res senses :test #'part=))
	    res)))))


(defun get-wordsenses (synset)
  (mapcar #'object (get-triples-list :s synset :p !wn30:containsWordSense)))

(defun rename-all-blank-wordsenses ()
  (let* ((table (mapcar #'car (run-query-as-list "wordsenses-with-blank-nodes.sparql")))
         (count 0)
         (len (length table)))
    (dolist (s table)
      (dolist (sense (get-wordsenses s))
	(if (blank-node-p sense)
	    (merge-nodes sense (make-new-wordsense-id s)))))))
