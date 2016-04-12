(ql:quickload :split-sequence)

(defpackage #:cstnews
  (:use #:cl :split-sequence))

(in-package :cstnews)


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
	     (cons (or (position (aref sa 0) (aref sa 4) :test #'equal :key #'car) -1)
		   ss)))
    (with-open-file (stream filename)
      (when stream
	(sort 
	 (mapcar (lambda (ss) (stats (translate ss)))
		 (loop for i = (read stream nil) while i append i))
	 #'(lambda (x y) (< (car x) (car y))))))))

