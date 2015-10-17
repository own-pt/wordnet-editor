
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

(defun make-wordsense-id (synset prefix n)
  (labels ((synset-type (synset)
	     (object (get-triple :s synset :p !rdf:type)))
	   (synset-id (synset)
	     (upi->value (object (get-triple :s synset :p !wn30:synsetId))))
	   (type-suffix (type)
	     (upi->value (object (get-triple :s type :p !wn30:suffixCode))))
	   (encode-synset (s)
	     (format nil "~a-~a"
		     (synset-id synset)
		     (type-suffix (synset-type synset)))))
    (upi (resource 
	  (format nil "~a-~a-~a" prefix (encode-synset synset) n)
	  "wn30pt"))))

(defun process-wordsense (synset blank prefix n)
  (merge-nodes blank (make-wordsense-id synset prefix n)))

(defun process-wordsenses (synset objects prefix n)
  (when objects
    (progn
      (process-wordsense synset (car objects) prefix n)
      (process-wordsenses synset (cdr objects) prefix (1+ n)))))
 
(defun get-wordsenses (synset)
  (mapcar #'object (get-triples-list :s synset :p !wn30:containsWordSense)))

(defun process-all-blank-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-with-blank-nodes.sparql"))))
    (dolist (s table)
      (process-wordsenses s (get-wordsenses s) "tmp-wordsense" 1))))

(defun rename-wordsenses ()
  (let ((table (mapcar #'car (run-query-as-list "wordsenses-without-blank-nodes.sparql"))))
    (dolist (s table)
      (process-wordsenses s (get-wordsenses s) "wordsense" 1))))

