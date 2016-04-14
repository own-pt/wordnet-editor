(in-package :cstnews)

(defparameter *query*
  "select ?ss 
  { ?a wn30:hyponymOf* ?ss .
    ?b wn30:hyponymOf* ?ss . }")

(defun common-ancestor (a b)
  (run-sparql (parse-sparql *query*
			    (alexandria:alist-hash-table (collect-namespaces)))
	      :with-variables `((?a . ,(resource (format nil "synset-~a" a) "wn30en"))
				(?b . ,(resource (format nil "synset-~a" b) "wn30en"))) 
	      :engine :sparql-1.1 :results-format :lists))


(defun file->list (filename &optional (type "v"))
  (labels ((translate (ss &aux
			  (len (length ss))
			  (sa (make-array len :initial-contents ss)))
	     (loop for x from 1 to len by 2
		   collect (case x
			     ((1)
			      (format nil "~8,'0d-~a" (parse-integer (aref sa x)) type))
			     ((9)
			      (mapcar (lambda (s) (split-sequence #\: s))
				      (split-sequence #\/ (aref sa x))))
			     (t (aref sa x)))))
	   (stats (ss &aux
		      (len (length ss))
		      (sa (make-array len :initial-contents ss)))
	     (list (or (position (aref sa 0) (aref sa 4) :test #'equal :key #'car) -1)
		   (some (lambda (a) (common-ancestor a (aref sa 0)))
			 (mapcar #'car (aref sa 4)))
		   ss)))
    (with-open-file (stream filename)
      (when stream
	(mapcar (lambda (ss) (stats (translate ss)))
		(loop for i = (read stream nil) while i append i))))))

