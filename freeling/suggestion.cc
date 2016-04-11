#include <iostream>
#include <map>

#include "freeling.h"
#include "freeling/morfo/traces.h"

using namespace std;
using namespace freeling;

void ProcessSentences(list<sentence> &ls) {
  sentence::const_iterator previous_w;
  sentence::const_iterator human_w;
  int state = 0;
  wcout << L"(";
  for (list<sentence>::iterator is=ls.begin(); is!=ls.end(); is++) {
    for (sentence::const_iterator w=is->begin(); w!=is->end(); w++) {
      if (state == 0)
	{
	  if (w->get_form()==L"<" && w->get_lemma()==L"<")
	    {
	      previous_w = w;
	      previous_w--;
	      
	      state = 1;
	    }
	}
      if (state == 1)
	{
	  if (w->get_form()==L">" && w->get_lemma()==L">" && w->get_tag() == L"Fz")
	    {
	      human_w = w;
	      human_w--;
	      state = 2;
	    }
	}
     
      if (state == 2)
	{
	  if (w->get_form()==L">" && w->get_lemma()==L">")
	    {
	      wcout << L"(:human-suggestion \""<< human_w->get_form() << L"\" "; 
	      wcout << L":form \""<< previous_w->get_form() << L"\" :lemma \"" << previous_w->get_lemma() << L"\" :tag \"" << previous_w->get_tag() << L"\" " << L" :synset-list \""<< previous_w->get_senses_string() << "\")"; 
	      state = 0;
	    }
	}
    }
  }
  wcout << L")";
}




int main (int argc, char **argv) {
  util::init_locale(L"default");

  wstring ipath;
  if(argc < 2) ipath=L"/usr/local";
  else ipath=util::string2wstring(argv[1]);

  wstring lang=L"pt";
  wstring path=ipath+L"/share/freeling/"+lang+L"/";

  tokenizer tk(path+L"tokenizer.dat"); 
  splitter sp(path+L"splitter.dat");

  maco_options opt(L"es");  
  opt.UserMapFile=L"";
  opt.LocutionsFile=path+L"locucions.dat"; opt.AffixFile=path+L"afixos.dat";
  opt.ProbabilityFile=path+L"probabilitats.dat"; opt.DictionaryFile=path+L"dicc.src";
  opt.NPdataFile=path+L"np.dat"; opt.PunctuationFile=path+L"../common/punct.dat"; 

  maco morfo(opt); 
  morfo.set_active_options (false,// UserMap
                             true, // NumbersDetection,
                             true, //  PunctuationDetection,
                             true, //  DatesDetection,
                             true, //  DictionarySearch,
                             true, //  AffixAnalysis,
                             false, //  CompoundAnalysis,
                             true, //  RetokContractions,
                             true, //  MultiwordsDetection,
                             true, //  NERecognition,
                             false, //  QuantitiesDetection,
                             true);  //  ProbabilityAssignment

  hmm_tagger tagger(path+L"tagger.dat", true, FORCE_TAGGER); 

  senses sen(path+L"senses.dat");
  ukb wsd(path+L"ukb.dat");
  wstring line,text;
  while (getline(wcin,line)) 
    text = text + line + L"\n";

  list<word> lw = tk.tokenize(text);
  list<sentence> ls = sp.split(lw);   
  morfo.analyze(ls);
  tagger.analyze(ls);
  sen.analyze(ls);
  wsd.analyze(ls);
 
  ProcessSentences(ls);    

}
