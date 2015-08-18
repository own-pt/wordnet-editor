;;; these are various functions that were used to clean up the own-pt triplestore
;;; that aren't needed anymore; they are retained here if we need to do similar cleanups
;;; in the future.
(in-package :wordnet)

(defun clean-up-word (str)
  (cl-ppcre:regex-replace-all "[^\\w]" str "_"))

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

;;; A quick word on these two functions:

;;; Initially, OpenWordnet-PT was in an inconsistent state where some synsets
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

(defun process-all-blank-pt-words (&key (ns "wn30pt"))
  (let ((result (run-query-as-list "pt-blank-words.sparql")))
    (dolist (w result)
      (let ((uri (resource 
		  (format nil "word-~a"
			  (clean-up-word (upi->value (cadr w)))) ns)))
        (merge-nodes (car w) uri)))))

(defun process-all-blank-en-words (&key (ns "wn30en"))
  (let ((result (run-query-as-list "en-blank-words.sparql")))
    (dolist (w result)
      (let ((uri (resource 
		  (format nil "word-~a"
			  (clean-up-word (upi->value (cadr w)))) ns)))
        (merge-nodes (car w) uri)))))

;;this hack was necessary because we had invalid URIs in the database
;;specifically a couple of words under http://arademaker.github.com/wn30-br/instances/
;;had spaces in them.  since none of the libraries that I tried support URIs with
;;spaces in them I had to resort to using SUBSEQ.
(defun process-all-words-with-invalid-chars (&key (ns "wn30pt"))
  (dolist (w (get-triples-list :p !rdf:type :o !wn30:Word))
    (when (not (blank-node-p (subject w)))
      (let ((word (upi->value (subject w))))
	(when (find #\space word)
	  (let ((uri (resource (format nil "word-~a"
				       (clean-up-word (subseq word 52))) ns)))
	    (merge-nodes (subject w) uri)))))))

;;;
;;; fixes the predicates synsetId and tagCount.  They need to make
;;; sure that the literals are properly typed as
;;; xsd:nonnegativeInteger
;;;
;;; https://w3id.org/own-pt/wn30/schema/tagCount
;;; https://w3id.org/own-pt/wn30/schema/synsetId
;;;
;;; xs:nonNegativeInteger

(defun fix-incorrectly-typed-literals1 (predicate)
  (dolist (tr (get-triples-list :p predicate))
    (let* ((s (subject tr))
           (p (predicate tr))
           (o (object tr))
           (g (graph tr))
           (tl (literal (upi->value o) :datatype !xs:nonNegativeInteger)))
      (delete-triple (triple-id tr))
      (add-triple s p tl :g g))))

(defun fix-incorrectly-typed-literals ()
  (fix-incorrectly-typed-literals1 !wn30:tagCount)
  (fix-incorrectly-typed-literals1 !wn30:synsetId))

