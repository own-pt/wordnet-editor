(in-package :utils)

(defclass suggestion ()
  ((human-suggestion  :reader human-suggestion
		      :initarg  :human-suggestion)
   (form :reader form
	     :initarg :form) 
   (lemma :reader lemma
	    :initarg :lemma)
   (tag :reader tag
	:initarg :tag)
   (synset-list :reader synset-list
     :initarg :synset-list)))

(defun delimiterp (c) (or (char= c #\:) (char= c #\/)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun synset-list->synset-alist (synset-list)
 (let* ((clean-list (my-split synset-list)))
   (loop for i from 0 to (1- (length clean-list)) append
	(if (= 0 (mod i 2))
	    (list (cons (nth i clean-list) (nth (1+ i) clean-list)))))))
 

(defun fix-synset (word &key(type "v") (synset-size 8))
  (concatenate 'string 
	       (make-string 
		(- synset-size (length word))
		:initial-element #\0)
	       word
	       "-" 
	       type))
