(in-package :wordnet)

(defun export-triples (triples file)  
  (with-open-file (output file  
                          :direction :output  
                          :if-does-not-exist :create  
                          :if-exists :error)  
    (print-triples triples  
                   :limit nil :stream output :format :ntriple)))

(defun export-all-old ()
  (export-triples (get-triples :g (default-graph-upi *db*)) "/tmp/e/default-graph.nt")
  (export-triples (get-triples :g !old-source:nomlex-pt.rdf.gz) "/tmp/e/nomlex.nt")
  (export-triples (get-triples :g !old-source:wnlite.turtle) "/tmp/e/wnlite.nt")
  (export-triples (get-triples :g !old-source:wordnet-en.ntriples.gz) "/tmp/e/wordnet-en.nt")
  (export-triples (get-triples :g !old-source:openWordnet-PT.rdf.gz) "/tmp/e/own-pt.nt"))

  
(defun export-all ()
  (export-triples (get-triples :g !source:wn30.ttl) "/tmp/e/wn30.nt")
  (export-triples (get-triples :g !source:morphosemantic-links.xlsx) "/tmp/e/morphosemantic-links.nt")
  (export-triples (get-triples :g !source:own-pt.nt) "/tmp/e/own-pt.nt")
  (export-triples (get-triples :g !source:wordnet-en.nt) "/tmp/e/wordnet-en.nt"))

  
