#!/bin/bash

## FIRST, CLEAN UP ALL THE WRONG IRIS (THAT HAVE SPACES ON THEM)
## BY EXECUTING WORDNET-EDITOR::PROCESS-ALL-WORDS-WITH-INVALID-CHARS

## THEN, EXECUTE WORDNET-EDITOR::EXPORT-TRIPLES and copy the files to original/ 

mkdir clean tmp new-namespace 2> /dev/null

rm -f clean/*
rm -f tmp/*
rm -f new-namespace/*

for f in original/*; do
    cat $f \
        | sed -e 's,http://arademaker.github.com/wn30-br/instances/,https://w3id.org/own-pt/wn30-pt/instances/,g' \
        | sed -e 's,http://arademaker.github.com/nomlex-br/instances/,https://w3id.org/own-pt/nomlex/instances/,g' \
        | sed -e 's,http://arademaker.github.com/nomlex/schema/,https://w3id.org/own-pt/nomlex/schema/,g' \
        | sed -e 's,http://arademaker.github.com/wn30/instances/,https://w3id.org/own-pt/wn30-en/instances/,g' \
        | sed -e 's,http://arademaker.github.com/wn30/schema/,https://w3id.org/own-pt/wn30/schema/,g' > new-namespace/`basename $f`
done

## Now import new-namespace/ into Allegro Graph, as follows:

## cd new-namespace
## agload -g "<file://own-pt.nt>" wn30 nomlex.nt
## agload -g "<file://wnlite.nt>" wn30 wnlite.nt
## agload -g "<file://own-pt.nt>" wn30 own-pt.nt
## agload -g "<file://wordnet-en.nt>" wn30 wordnet-en.nt
## agload wn30 default-graph.nt

## Now execute WORDNET-EDITOR::FIX-DEFAULT-GRAPH
