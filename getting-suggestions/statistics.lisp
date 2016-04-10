(in-package :statistics)

(defun how-many-members? (suggestion-list)
  (loop for i in suggestion-list sum
       (if (member (fix-synset (human-suggestion i)) 
		   (synset-list->synset-alist
		    (synset-list i)) :test #'(lambda(x y) (string= x (car y)))) 1 0)))

(defun members-percentage (suggestion-list)
  (/ (how-many-members? suggestion-list) (length suggestion-list)))

;;(members-percentage (generate-suggestions (suggestion-file->list)))
