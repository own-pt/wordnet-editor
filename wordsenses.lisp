
(in-package :wordnet)

;;; Initially, OpenWordnet-PT was in an inconsitent state where some synsets
;;; had "containsWordSense" relations with blank nodes AND regular nodes.
;;; To avoid any name conflict, we simply opted to rename ALL objects (regardless of
;;; whether they are blank or not) to "tmp-wordsense-<synset-id>-<number>" and then
;;; rename ALL these new nodes to its final value "wordsense-<synset-id>-<number>".
;;;
;;; So, given an inconsistent OpenWordnet-PT, the following methods need to be called
;;; in the correct order:
;;;
;;; (process-all-blank-wordsenses)
;;; 
;;; at this point all synsets will have "containsWordSense" pointing
;;; to "tmp-wordsense-xxx-yyy" nodes
;;;
;;; (rename-wordsenses)
;;;
;;; at this point all "tmp-wordsense-*" will be renamed to "wordsense-*".
;;;
;;; Since there was no information about the order of words in the
;;; first place, it doesn't matter for these methods the final order
;;; of the words.

(defun make-new-wordsense-id (synset)
  (labels ((new-part (synset n)
	     (resource (cl-ppcre:regex-replace "synset-([0-9]{8}-[anvs])"
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


(defun process-all-blank-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-with-blank-nodes.sparql"))))
    (dolist (s table)
      (dolist (sense (get-wordsenses s))
	(if (blank-node-p sense)
	    (merge-nodes sense (make-new-wordsense-id s)))))))
