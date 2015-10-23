(in-package :wordnet)

(defun export-triples (triples file)  
  (with-open-file (output file  
                          :direction :output  
                          :if-does-not-exist :create  
                          :if-exists :supersede)  
    (print-triples triples  
                   :limit nil :stream output :format :ntriple)))

(defun get-triples-from-list (list)
  (mapcar (lambda (x) (get-triple :s (first x) :p (second x) :o (third x))) list))

(defun export-all ()
  (export-triples (get-triples :g !source:wn30.ttl) "/tmp/wn30.nt")
  (export-triples (get-triples-from-list (run-query-as-list "morphosemantic-en.sparql"))
                  "/tmp/morphosemantic-links-en.nt")
  (export-triples (get-triples-from-list (run-query-as-list "morphosemantic-pt.sparql"))
                  "/tmp/morphosemantic-links-pt.nt")
  (export-triples (get-triples :g !source:own-pt.nt) "/tmp/own-pt.nt")
  (export-triples (get-triples :g !source:wordnet-en.nt) "/tmp/wordnet-en.nt"))

  
