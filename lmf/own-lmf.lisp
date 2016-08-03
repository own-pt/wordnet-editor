;; (load "/usr/local/agraph-client/agraph")
(ql:quickload :alexandria)
(ql:quickload :split-sequence)
(ql:quickload :xml-emitter)

(defpackage #:own-pt-lmf
  (:use #:cl :excl
        :split-sequence :xml-emitter
	:db.agraph :prolog :db.agraph.sparql :net.uri))

(in-package :own-pt-lmf)

(enable-!-reader)
(enable-print-decoded t)

(register-namespace "wn30"   "https://w3id.org/own-pt/wn30/schema/" :errorp nil)
(register-namespace "wn30en" "https://w3id.org/own-pt/wn30-en/instances/" :errorp nil)
(register-namespace "wn30pt" "https://w3id.org/own-pt/wn30-pt/instances/" :errorp nil)
(register-namespace "nomlex" "https://w3id.org/own-pt/nomlex/schema/" :errorp nil)
(register-namespace "nm-pt"  "https://w3id.org/own-pt/nomlex/instances/" :errorp nil)
(register-namespace "skos"   "http://www.w3.org/2004/02/skos/core#" :errorp nil)
(register-namespace "prov"   "http://www.w3.org/ns/prov#" :errorp nil)
(register-namespace "rdfs"   "http://www.w3.org/2000/01/rdf-schema#" :errorp nil)
(register-namespace "source" "file://" :errorp nil)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun query-string (query-name)
  (file-string (make-pathname :name query-name)))

(defun run-query-as-list (query-name)
  (run-sparql (parse-sparql (query-string query-name)
			    (alexandria:alist-hash-table (collect-namespaces)))
	      :engine :sparql-1.1 :results-format :lists))

(defparameter *ili-map* nil)
(defparameter *synsets* nil)
(defparameter *glosses* nil)
(defparameter *examples* nil)
(defparameter *synset-relations* nil)
(defparameter *senses* nil)
(defparameter *sense-relations* nil)

;; Lemma
;;     partOfSpeech (n v a r s t c p x u)

;; SenseRelation relType (antonym also participle pertainym derivation
;;     domain_topic has_domain_topic domain_region has_domain_region
;;     exemplifies is_exemplified_by similar other)

;; Synset
;;     partOfSpeech (n v a r s t c p x u) #IMPLIED

;; SynsetRelation relType (agent also attribute be_in_state causes
;;     classified_by classifies co_agent_instrument co_agent_patient
;;     co_agent_result co_instrument_agent co_instrument_patient
;;     co_instrument_result co_patient_agent co_patient_instrument
;;     co_result_agent co_result_instrument co_role direction
;;     domain_region domain_topic exemplifies entails eq_synonym
;;     has_domain_region has_domain_topic is_exemplified_by
;;     holo_location holo_portion holo_substance
;;     holonym hypernym hyponym in_manner instance_hypernym
;;     instance_hyponym instrument involved involved_agent
;;     involved_direction involved_instrument involved_location
;;     involved_patient involved_result involved_source_direction
;;     involved_target_direction is_caused_by is_entailed_by location
;;     manner_of mero_location mero_part mero_portion
;;     mero_substance meronym similar other patient restricted_by
;;     restricts result role source_direction state_of
;;     target_direction subevent is_subevent_of antonym)

(defun convert-to-POS (type)
  (cond 
    ((part= type !wn30:VerbSynset) "v")
    ((part= type !wn30:NounSynset) "n")
    ((part= type !wn30:AdjectiveSynset) "a")
    ((part= type !wn30:AdjectiveSatelliteSynset) "a")
    ((part= type !wn30:AdverbSynset) "r")))

(defun fill-ili-map ()
  (setf *ili-map* (make-hash-table :test #'equal))
  (with-open-file (stream "ili-map.csv")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let* ((cols (split-sequence #\, line))
             (ili (first cols))
             (pwn30 (format nil "own-pt-~a" (second cols))))
        (setf (gethash pwn30 *ili-map*) ili)))))

(defun fill-synsets ()
  (setf *synsets* nil)
  (dolist (tr (run-query-as-list "synsets.sparql"))
    (let* ((synset-id (first tr))
           (type (convert-to-POS (second tr)))
           (lexfile (third tr))
           (id (make-synset-id (part->terse synset-id) type)))
      (push (list id type (part->terse lexfile)) *synsets*))))

(defun fill-glosses ()
  (setf *glosses* (make-hash-table :test #'equal))
  (dolist (tr (run-query-as-list "glosses.sparql"))
    (let* ((gloss (first tr))
           (synset-id (second tr))
           (type (third tr))
           (id (make-synset-id (part->terse synset-id) (convert-to-POS type))))
      (push (part->terse gloss) (gethash id *glosses*)))))

(defun fill-examples ()
  (setf *examples* (make-hash-table :test #'equal))
  (dolist (tr (run-query-as-list "examples.sparql"))
    (let* ((example (first tr))
           (synset-id (second tr))
           (type (third tr))
           (id (format nil "own-pt-~a-~a" (part->terse synset-id) (convert-to-POS type))))
      (push (part->terse example) (gethash id *examples*)))))

;; SynsetRelation relType (agent also attribute be_in_state causes
;;     classified_by classifies co_agent_instrument co_agent_patient
;;     co_agent_result co_instrument_agent co_instrument_patient
;;     co_instrument_result co_patient_agent co_patient_instrument
;;     co_result_agent co_result_instrument co_role direction
;;     exemplifies eq_synonym
;;     is_exemplified_by
;;     holo_location holo_member holo_part holo_portion holo_substance
;;     holonym in_manner 
;;     instrument involved involved_agent
;;     involved_direction involved_instrument involved_location
;;     involved_patient involved_result involved_source_direction
;;     involved_target_direction is_caused_by is_entailed_by location
;;     manner_of mero_location mero_member mero_portion
;;     mero_substance meronym other patient restricted_by
;;     restricts result role source_direction state_of
;;     target_direction subevent is_subevent_of antonym)


;; this list was made by matching the conversion of PWN30 pointers in
;; https://github.com/own-pt/wordnet2rdf/blob/master/common.lisp
;; to the conversion of PWN30 pointers in https://github.com/jmccrae/gwn-scala-api/blob/master/src/main/scala/org/globalwordnet/wnapi/wndb.scala

(defparameter *conversion-table* `((!wn30:hypernymOf "hypernym")
                                   (!wn30:hyponymOf "hyponym")
                                   (!wn30:instanceOf "instance_hypernym") 
                                   (!wn30:hasInstance "instance_hyponym")
                                   (!wn30:memberMeronymOf "mero_member") ;; MemberMeronym/member_meronym in the Scala, but mero_member in the DTD
                                   (!wn30:memberHolonymOf "holo_member") ;; MemberHolonym/member_holonym in the Scala, but holo_member in the DTD
                                   (!wn30:similarTo "similar")
                                   (!wn30:entails "entails") ;; Entailment/entail in the Scala, but entails in the DTD
                                   (!wn30:substanceMeronymOf "mero_substance") ;; SubstanceMeronym/substance_meronym in the Scala, mero_substance in the DTD
                                   (!wn30:substanceHolonymOf "holo_substance") ;; SubstanceHolonym/substance_holonym in the Scala, holo_substance in the DTD
                                   (!wn30:partMeronymOf "mero_part") ;; PartMeronym/part_meronym vs mero_part in DTD
                                   (!wn30:partHolonymOf "holo_part") ;; PartHolonym/part_holony vs holo_part in DTD
                                   (!wn30:classifiedByTopic "domain_topic") ;; DomainOfSynsetTopic/domain_category vs domain_topic in DTD
                                   (!wn30:classifiedByRegion "domain_region")
                                   (!wn30:classifiedByUsage "exemplifies") ;; DomainOfSynsetUsage/domain_usage, but no similar string in DTD
                                   (!wn30:classifiesByTopic "has_domain_topic") ;; MemberOfThisDomainTopic/domain_member_category, has_domain_topic in DTD
                                   (!wn30:classifiesByRegion "has_domain_region") ;; MemberOfThisDomainRegion/domain_member_region, has_domain_region in DTD
                                   (!wn30:classifiesByUsage "is_exemplified_by")
                                   (!wn30:derivationallyRelated "derivation")
                                   (!wn30:causes "causes") ;; Cause/cause vs causes in DTD
                                   (!wn30:sameVerbGroupAs "similar")
                                   (!wn30:attribute "attribute")
                                   (!wn30:antonymOf "antonym")
                                   (!wn30:seeAlso "also")
                                   (!wn30:participleOf "participle")
                                   (!wn30:adjectivePertainsTo "pertainym")
                                   (!wn30:adverbPertainsTo "derivation")
                                   (!nomlex:byMeansOf "by_means_of")
                                   (!nomlex:location "location")
                                   (!nomlex:uses "uses")
                                   (!nomlex:bodyPart "body_part")
                                   (!nomlex:undergoer "undergoer")
                                   (!nomlex:property "property")
                                   (!nomlex:vehicle "vehicle")
                                   (!nomlex:agent "agent")
                                   (!nomlex:result "result")
                                   (!nomlex:material "material")
                                   (!nomlex:event "event")
                                   (!nomlex:instrument "instrument")
                                   (!nomlex:destination "destination")
                                   (!nomlex:relation "relation")
                                   (!nomlex:state "state")))

(defun convert-relation (type)
  (second (assoc type *conversion-table* :test #'part=)))

(defun fill-synset-relations ()
  (setf *synset-relations* (make-hash-table :test #'equal))
  (dolist (tr (run-query-as-list "synset-relations.sparql"))
    (let* ((s1 (first tr))
           (t1 (second tr))
           (rel (third tr))
           (s2 (fourth tr))
           (t2 (fifth tr))
           (id1 (make-synset-id (part->terse s1) (convert-to-POS t1)))
           (id2 (make-synset-id (part->terse s2) (convert-to-POS t2)))
           (relation (convert-relation rel)))
      (push (cons relation id2) (gethash id1 *synset-relations*)))))

(defun fill-senses ()
  (setf *senses* (make-hash-table :test #'equal))
  (dolist (tr (run-query-as-list "lexical-forms.sparql"))
    (let* ((synset-id (first tr))
           (type (second tr))
           (pos (convert-to-POS type))
           (lexical-form (part->terse (third tr)))
           (wordsense (fourth tr))
           (lexical-form-id (make-synset-id lexical-form pos))
           (id (part->terse synset-id)))
      (push `(,id ,wordsense ,lexical-form ,pos) (gethash lexical-form-id *senses*)))))

;; select ?lf1 ?id1 ?t1 ?p ?lf2 ?id2 ?t2
(defun fill-sense-relations ()
  (setf *sense-relations* (make-hash-table :test #'equal))
  (dolist (tr (run-query-as-list "sense-relations.sparql"))
    (let* ((lf1 (part->terse (first tr)))
           (id1 (part->terse (second tr)))
           (pos1 (convert-to-POS (third tr)))
           (rel (convert-relation (fourth tr)))
           (lf2 (part->terse (fifth tr)))
           (id2 (part->terse (sixth tr)))
           (pos2 (convert-to-POS (seventh tr)))
           (sense-id1 (make-sense-id id1 pos1 lf1))
           (sense-id2 (make-sense-id id2 pos2 lf2)))
      (push (cons sense-id2 rel) (gethash sense-id1 *sense-relations*)))))

(defun fill-data ()
  (fill-ili-map)
  (fill-synsets)
  (fill-glosses)
  (fill-examples)
  (fill-synset-relations)
  (fill-senses)
  (fill-sense-relations))

(defun combine-gloss-and-examples (glosses examples)
  (format nil "~{~a~^ ~};~{~a~^;~}" glosses examples))

(defun make-synset-id (id pos)
  (format nil "own-pt-~a-~a" id pos))

(defun make-sense-id (id pos word)
  (format nil "own-pt-~a-~a-~a" id pos word))

(defun output-lmf ()
  (with-open-file (stream "own-pt.xml" :direction :output :if-exists :supersede)
    (with-xml-output (stream :encoding "UTF-8")
      (with-tag ("LexicalResource" '(("xmlns:dc" "http://purl.org/dc/elements/1.1/")))
        (with-tag ("Lexicon"
                   '(("id" "ownpt")
                     ("label" "OpenWordnet-PT")
                     ("language" "pt")
                     ("email" "alexrad@br.ibm.com")
                     ("license" "http://creativecommons.org/licenses/by/4.0/")
                     ("version" "d65c922")
                     ("url" "http://openwordnet-pt.org/">)))

          (maphash (lambda (k vs)
                     (with-tag ("LexicalEntry" `(("id" ,k)))
                       (dolist (v vs)
                         (with-tag ("Lemma" `(("writtenForm" ,(third v))
                                              ("partOfSpeech" ,(fourth v))))
                           (let ((sense-id (make-sense-id (first v) (fourth v) (third v))))
                             (with-tag ("Sense" `(("id" ,sense-id)
                                                  ("synset" ,(make-synset-id (first v) (fourth v)))))
                               (let ((sense-relations (gethash sense-id *sense-relations*)))
                                 (when sense-relations
                                   (dolist (sr sense-relations)
                                     (with-simple-tag ("SenseRelation" `(("target" ,(car sr))
                                                                         ("relType" ,(cdr sr)))))))))))))) 
                   *senses*)
          
          (dolist (s *synsets*)
            (let ((synset-id (first s))
                  (type (second s))
                  (lex-file (third s)))
             (with-tag ("Synset" `(("id" ,synset-id)
                                   ("ili" ,(gethash synset-id *ili-map*))
                                   ("partOfSpeech" ,type)
                                   ("dc:subject" ,lex-file)))
               (let ((glosses (gethash s *glosses*))
                     (examples (gethash s *examples*))
                     (relations (gethash s *synset-relations*)))
                 (when glosses
                   (with-tag ("Definition")
                     (xml-out (combine-gloss-and-examples glosses examples))))
                 (when relations
                   (dolist (r relations)
                     (with-tag ("SynsetRelation" `(("target" ,(cdr r))
                                                   ("relType" ,(car r))))))))))))))))
