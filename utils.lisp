
(in-package :wordnet)

(defvar *package-dir* (pathname-directory
		       (asdf:system-definition-pathname
			(asdf:find-system :wordnet-editor))))

(defvar *queries-dir* (append *package-dir* (list "queries")))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun query-string (query-name)
  (file-string (make-pathname :directory *queries-dir* 
			      :name query-name)))

(defun run-query-as-list (query-name)
  (run-sparql (parse-sparql (query-string query-name)
			    (alexandria:alist-hash-table (collect-namespaces)))
	      :engine :sparql-1.1 :results-format :lists))
