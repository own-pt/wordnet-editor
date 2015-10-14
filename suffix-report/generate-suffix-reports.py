#!/usr/bin/env python

import unicodecsv as csv

from ptstemmer.implementations.OrengoStemmer import OrengoStemmer
from ptstemmer.implementations.SavoyStemmer import SavoyStemmer
from ptstemmer.implementations.PorterStemmer import PorterStemmer

if __name__ == '__main__':
    stemmers = [OrengoStemmer(), PorterStemmer(), SavoyStemmer()]
    
    with open('morphosemantic-links-words.csv') as csvfile:
        words = csv.reader(csvfile, delimiter=',')
        for r in words:
            [w1,r,w2] = [w.strip().encode('latin-1') for w in r]
            s1 = w1.split(stemmers[0].getWordStem(w1))[1]
            s2 = w2.split(stemmers[0].getWordStem(w2))[1]
            print "%s, %s, %s, %s, %s" % (w1, r, w2, s1, s2)
