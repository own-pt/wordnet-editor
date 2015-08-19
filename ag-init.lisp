;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(in-package :wordnet)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "old-wn30"   "http://arademaker.github.com/wn30/schema/" :errorp nil)
(register-namespace "old-source" "file:///home/ubuntu/wordnet/")

(register-namespace "wn30"   "https://w3id.org/own-pt/wn30/schema/" :errorp nil)
(register-namespace "wn20"   "http://www.w3.org/2006/03/wn/wn20/schema/" :errorp nil)
(register-namespace "wn30en" "https://w3id.org/own-pt/wn30-en/instances/" :errorp nil)
(register-namespace "wn30pt" "https://w3id.org/own-pt/wn30-pt/instances/" :errorp nil)
(register-namespace "nomlex" "https://w3id.org/own-pt/nomlex/schema/" :errorp nil)
(register-namespace "nm-pt"  "https://w3id.org/own-pt/nomlex/instances/" :errorp nil)
(register-namespace "skos"   "http://www.w3.org/2004/02/skos/core#" :errorp nil)
(register-namespace "source" "file://")
