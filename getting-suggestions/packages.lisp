
(defpackage :utils
  (:use :cl)
  (:export
   #:suggestion
   #:my-split
   #:synset-list->synset-alist
   #:fix-synset
   #:delimiterp
   #:human-suggestion
   #:form
   #:lemma
   #:tag
   #:synset-list))


(defpackage :suggestion-reader
  (:use :cl :utils)
  (:export
   #:generate-suggestions
   #:suggestion-file->list))

(defpackage :statistics
  (:use :cl :utils :suggestion-reader))
