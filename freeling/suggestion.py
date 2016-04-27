#! /usr/bin/python3

### REQUIRES python 3 !!!!
## For example:
##     ./suggestion.py <test.txt >test_out.txt

import freeling
import sys

FREELINGDIR = "/usr/local";

DATA = FREELINGDIR+"/share/freeling/";
LANG="pt";

freeling.util_init_locale("default");

# create language analyzer
la=freeling.lang_ident(DATA+"common/lang_ident/ident.dat");

# create options set for maco analyzer. Default values are Ok, except for data files.
op= freeling.maco_options("pt");
op.set_data_files( "", 
                   DATA + "common/punct.dat",
                   DATA + LANG + "/dicc.src",
                   DATA + LANG + "/afixos.dat",
                   "",
                   DATA + LANG + "/locucions.dat", 
                   DATA + LANG + "/np.dat",
                   DATA + "common/quantities_default.dat",
                   DATA + LANG + "/probabilitats.dat");

# create analyzers
tk=freeling.tokenizer(DATA+LANG+"/tokenizer.dat");
sp=freeling.splitter(DATA+LANG+"/splitter.dat");
sid=sp.open_session();
mf=freeling.maco(op);

# activate mmorpho odules to be used in next call
mf.set_active_options(False, True, True, True,  # select which among created 
                      True, True, False, True,  # submodules are to be used. 
                      True, True, True, True ); # default: all created submodules are used

# create tagger, sense anotator, and parsers
tg=freeling.hmm_tagger(DATA+LANG+"/tagger.dat",True,2);
sen=freeling.senses(DATA+LANG+"/senses.dat");
parser= freeling.chart_parser(DATA+LANG+"/chunker/grammar-chunk.dat");
wsd = freeling.ukb(DATA+LANG+"/ukb.dat");
# dep=freeling.dep_txala(DATA+LANG+"/dep/dependences.dat", parser.get_start_symbol());

# process input text
lin=sys.stdin.readline();
print("(")
#XSprint ("Text language is: "+la.identify_language(lin,["es","ca","en","it"])+"\n");
state = 0;
while (lin) :
        
    l = tk.tokenize(lin);
    ls = sp.split(sid,l,False);

    ls = mf.analyze(ls);
    ls = tg.analyze(ls);
    ls = sen.analyze(ls);
    ls = parser.analyze(ls);
    ls = wsd.analyze(ls);
    # ls = dep.analyze(ls);
    tempo = 0
    ## output results
    for s in ls :
       ws = s.get_words();
       for w in ws :
           if(tempo == 0):
               human = w
               previous_helper = w
           tempo = tempo+1
           
           if (state == 0):
               if (w.get_form() == "<" and w.get_lemma()=="<" and w.get_tag() == "Fz"):
                   state = 1
           if (state == 1):
               if (w.get_form() == ">" and w.get_lemma()==">" and w.get_tag() == "Fz"):
                   state = 2
           if (state == 2):
               if (w.get_form() == ">" and w.get_lemma()==">"):
                   print("(:human-suggestion \""+human.get_form()+"\" :form \""+previous.get_form()+"\" :lemma \""+previous.get_lemma()+"\" :tag \""+previous.get_tag()+"\" :synset-list \""+previous.get_senses_string()+"\")")
                   state=0
                   print("")
           previous = previous_helper
           previous_helper = human
           human = w
 #          print(w.get_form()+" "+w.get_lemma()+" "+w.get_tag()+" "+w.get_senses_string());
      # print ("");
       
#       tr = s.get_parse_tree();
#       printTree(tr, 0);

 #      dp = s.get_dep_tree();
 #      printDepTree(dp, 0)

    lin=sys.stdin.readline();
print(")")    
# clean up       
sp.close_session(sid);
    
