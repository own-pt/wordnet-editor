
(in-package :wordnet)

(defvar *package-dir* (pathname-directory (asdf:system-definition-pathname (asdf:find-system :wordnet))))
(defvar *queries-dir* (append *package-dir* (list "queries")))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun query-string (query-name)
  (file-string (make-pathname :directory *queries-dir* 
			      :name query-name)))

