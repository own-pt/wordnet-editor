;; (C) 2013 IBM Corporation
;; Author: Alexandre Rademaker

(in-package :wordnet)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "wn30"   "http://arademaker.github.com/wn30/schema/" :errorp nil)
(register-namespace "wn20"   "http://www.w3.org/2006/03/wn/wn20/schema/" :errorp nil)
(register-namespace "wn30en" "http://arademaker.github.com/wn30/instances/" :errorp nil)
(register-namespace "wn30br" "http://arademaker.github.com/wn30-br/instances/" :errorp nil)
(register-namespace "nomlex" "http://arademaker.github.com/nomlex/schema/" :errorp nil)
(register-namespace "nm-pt"  "http://arademaker.github.com/nomlex-br/instances/" :errorp nil)
(register-namespace "swn30"  "http://arademaker.github.com/swn30/schema/" :errorp nil)
(register-namespace "source" "file:///home/ubuntu/wordnet-br/")



