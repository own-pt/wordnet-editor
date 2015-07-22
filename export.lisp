(in-package :wordnet)

(defun export-triples (triples file)  
  (with-open-file (output file  
                          :direction :output  
                          :if-does-not-exist :create  
                          :if-exists :error)  
    (print-triples triples  
                   :limit nil :stream output :format :ntriple)))

(defun export-all ()
  (export-triples (get-triples :g (default-graph-upi *db*)) "/tmp/e/default-graph.nt")
  (export-triples (get-triples :g !source:nomlex-pt.rdf.gz) "/tmp/e/nomlex.nt")
  (export-triples (get-triples :g !source:wnlite.turtle) "/tmp/e/wnlite.nt")
  (export-triples (get-triples :g !source:wordnet-en.ntriples.gz) "/tmp/e/wordnet-en.nt")
  (export-triples (get-triples :g !source:openWordnet-PT.rdf.gz) "/tmp/e/own-pt.nt"))

  
