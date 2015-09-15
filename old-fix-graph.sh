
## [WNLITE.NT]
## copy wnlite unmodified
cp new-namespace/wnlite.nt clean/

## [WORDNET-EN.NT]
## copy wordnet-en unmodified
cp new-namespace/wordnet-en.nt clean/

## [NOMLEX.NT]
## copy nomlex unmodified
cat new-namespace/nomlex.nt > clean/own-pt.nt

## [DEFAULT-GRAPH.NT]
## split into a couple of sub-components

grep '^<http://logics.emap.fgv.br/wn/>' new-namespace/default-graph.nt > tmp/1
grep -v '^<http://logics.emap.fgv.br/wn/>' new-namespace/default-graph.nt > tmp/not-1

grep '^<https://w3id.org/own-pt/nomlex/' tmp/not-1 > tmp/2
grep -v '^<https://w3id.org/own-pt/nomlex/' tmp/not-1 > tmp/not-2

grep '^<https://w3id.org/own-pt/wn30-en/' tmp/not-2 > tmp/3
grep -v '^<https://w3id.org/own-pt/wn30-en/' tmp/not-2 > tmp/not-3

grep '^<https://w3id.org/own-pt/wn30-pt/' tmp/not-3 > tmp/4
grep -v '^<https://w3id.org/own-pt/wn30-pt/' tmp/not-3 > tmp/not-4

grep '^<https://w3id.org/own-pt/wn30/schema/' tmp/not-4 > tmp/5
grep -v '^<https://w3id.org/own-pt/wn30/schema/' tmp/not-4 > tmp/not-5

grep '^<http://wordnet.princeton.edu/' tmp/not-5 > tmp/6
grep -v '^<http://wordnet.princeton.edu/' tmp/not-5 > tmp/not-6

if [ `wc -l tmp/? | grep total | awk '{print $1}'` != `wc -l new-namespace/default-graph.nt |  awk '{print $1}'` ]
then
    echo "Error extracting different components from DEFAULT-GRAPH.  Aborting."
    exit 1;
else
    echo "DEFAULT-GRAPH extraction successful. Continuing."
fi

cat tmp/1 >> clean/own-pt.nt
cat tmp/2 >> clean/own-pt.nt
cat tmp/3 >> clean/wordnet-en.nt
cat tmp/4 >> clean/own-pt.nt
cat tmp/5 >> clean/suffixes.nt
cat tmp/6 >> clean/wordnet-en.nt

## [OWN-PT] 
## all the sameAs predicates are in own-pt.nt, move them to another
## file and keep a temporary version that doesn't have them
fgrep 'http://www.w3.org/2002/07/owl#sameAs' new-namespace/own-pt.nt > clean/same-as.nt

## the remainder of own-pt can be copied as-is, provided that it does
## not carry any blank node!
fgrep -v 'http://www.w3.org/2002/07/owl#sameAs' new-namespace/own-pt.nt >> tmp/own-pt.nt

if fgrep -q '_:' tmp/own-pt.nt 
then
    echo "Blank nodes found in new-namespace/own-pt.nt. Aborting."
    exit 1
fi

cat tmp/own-pt.nt >> clean/own-pt.nt

wc new-namespace/*
wc clean/*


echo "Now, import the files into Allegro Graph."
