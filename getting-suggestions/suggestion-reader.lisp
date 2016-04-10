(in-package :suggestion-reader)

(defun suggestion-file->list (&key (file-input "cstnews-verb-suggestions.txt"))
  (with-open-file (stream file-input)
    (when stream 
      (loop for i = (read stream nil) 
	 while i append i))))

(defun generate-suggestions (alist)
  (loop for i in alist collect
       (eval `(make-instance 'suggestion ,@i))))

