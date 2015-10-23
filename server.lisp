
(in-package :wordnet)

(defparameter *working-dir* (pathname-directory 
			     (asdf:system-definition-pathname 
			      (asdf:find-system :wordnet))))

(defparameter *solr* (make-instance 'solr:solr :uri "http://localhost:8983/solr/wn"))
(defparameter *solr-pointers* (make-instance 'solr:solr :uri "http://localhost:8983/solr/pointers"))

(webaction-project "wordnet"
		   :destination "site/"
		   :index "home"
		   :map
		   '(("home"  "home.clp")
		     ("search" do-query-text)
		     ("synset" "synset.clp")
		     ("get-synset" do-query-id)))


(defun valid-login? (user passwd)
  (and (equal user "test")
       (equal passwd "test")))


(defun do-query-text (req ent)
  (let ((term (request-query-value "term" req)))
    (if term
	(setf (request-variable-value req "result") 
	      (solr:solr-query *solr* :query term :fields "*" 
			       :param-alist '((:df . "text")
					      (:wt . "json")))))
    "home"))

(defun do-query-id (req ent)
  (let ((term (request-query-value "term" req)))
    (if term
	(setf (request-variable-value req "result") 
	      (solr:solr-query *solr* :query term :fields "*" 
			       :param-alist '((:df . "id")
					      (:wt . "json")))))
    "synset"))


(defun synset-html-1 (doc)
  (html ((:tr class "synset") 
	 ((:td class "id") 
	  ((:a href (format nil "get-synset?term=~a" (st-json:getjso "id" doc))) 
	   (:princ (st-json:getjso "id" doc))))
	 ((:td class "words") 
	  (:princ (format nil "~{~a~^, ~}" (st-json:getjso "word_br" doc)))) 
	 ((:td class "gloss") 
	  (let ((name (if (st-json:getjso "gloss_br" doc)
			  "gloss_br" 
			  "gloss_en")))
	    (html (:princ (st-json:getjso name doc))))))))


(defun synset-html-2 (doc)
  (html ((:p class "synset") 
	 ((:span class "id") (:princ (st-json:getjso "id" doc))) 
	 ((:span class "domain") (:princ (st-json:getjso "wn30_lexicographerFile" doc)))
	 ((:span class "word") (:princ (format nil "~{~a~^, ~}" (st-json:getjso "word_br" doc)))) 
	 ((:span class "word") (:princ (format nil "~{~a~^, ~}" (st-json:getjso "word_en" doc))))
	 ((:span class "gloss") (:princ (st-json:getjso "gloss_br" doc)))
	 ((:span class "gloss") (:princ (st-json:getjso "gloss_en" doc)))
	 ((:span class "link") (:princ (st-json:getjso "wn30_instanceOf" doc))))))


(def-clp-function display_data (req ent args body)
  (declare (ignore args ent body))
  (let* ((raw-result (request-variable-value req "result"))
	 (data (and raw-result 
		    (st-json:read-json raw-result))))
    (when data
      (html (synset-html-2 (car (st-json:getjso "docs" (st-json:getjso "response" data))))))))


(def-clp-function display_result (req ent args body)
  (declare (ignore args ent body))
  (let* ((raw-result (request-variable-value req "result"))
	 (data (and raw-result
		    (st-json:read-json raw-result))))
    (when data 
      (let ((ndoc (st-json:getjso "numFound" (st-json:getjso "response" data)))) 
	(html 
	 (:p "I found " (:princ ndoc) " documents.")
	 (:table 
	  (dolist (d (st-json:getjso "docs" (st-json:getjso "response" data)))
	    (synset-html-1 d))))))))


;; (defun page-header (req ent old-time old-val)
;;   (declare (ignore req ent old-time old-val))
;;   (with-output-to-string (p)
;;      (html-stream p 
;; 		  (:head (:title "openWordnet-PT Editor")))))

;; (defun page-footer (req ent old-time old-val)
;;   (declare (ignore req ent old-time old-val))
;;   (with-output-to-string (p)
;;     (html-stream p 
;; 		 ((:div :id "footer") 
;; 		  ((:p) 
;; 		   ((:a :href "/about.html") "More about") 
;; 		   " this site."
;; 		   " teste.")))))

;; (defun do-search (req ent)
;;   (with-http-response (req ent)
;;     (with-http-body (req ent)
;;       (html (:html (:head (:title "openWordnet-PT Editor"))
;; 		   (:body (:h2 "Welcome to the openWordnet-PT Editor")
;; 			  ((:form :method "get" :action "/search") 
;; 			   (:p "Lookup Word (or synset)" 
;; 			       ((:input :type "text" :name "term" :value "" :size "10" :maxlength "30"))))))))))

;; (publish :path "/search"
;; 	 :content-type "text/html; charset=utf-8;"
;; 	 :function #'do-search)

;; (publish-directory :prefix "/static/" 
;; 		   :destination (namestring (make-pathname :directory 
;; 							   (append *working-dir* (list "static")))))

;; (publish-file :path "/"
;; 	      :content-type "text/html; charset=utf-8;"
;; 	      :file (namestring (make-pathname :name "search" :type "html" 
;; 					       :directory (append *working-dir* (list "web")))))


;; (publish-multi :path "/test"
;; 	       :content-type "text/html; charset=utf-8;"
;; 	       :items (list #'page-header
;; 			    #'page-footer))
