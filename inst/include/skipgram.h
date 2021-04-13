#include "lib.h"
#include "dev.h"
using namespace quanteda;


inline unsigned int ngram_id(const Ngram &ngram,
                      MapNgrams &map_ngram,
                      IdNgram &id_ngram){
    
    auto it1 = map_ngram.find(ngram);
    if (it1 != map_ngram.end()) return it1->second;
#if QUANTEDA_USE_TBB    
    auto it2 = map_ngram.insert(std::pair<Ngram, unsigned int>(ngram, id_ngram.fetch_and_increment()));
#else
    auto it2 = map_ngram.insert(std::pair<Ngram, unsigned int>(ngram, id_ngram++));
#endif
    return it2.first->second;
    
}

inline void skip(const Text &tokens,
                 Text &tokens_ng,
                 const SetNgrams &set_words,
                 const unsigned int &start,
                 const unsigned int &n, 
                 const std::vector<unsigned int> &skips,
                 Ngram ngram,
                 MapNgrams &map_ngram,
                 IdNgram &id_ngram) {
    
    ngram.push_back(tokens[start]);
    
    //Rcout << "Size " << tokens.size() << "\n";
    //Rcout << "Token " << tokens[start] << "\n";
    
    if (ngram.size() < n) {
        for (std::size_t j = 0; j < skips.size(); j++) {
            unsigned int next = start + skips[j];
            if(tokens.size() - 1 < next) break;
            if(tokens[next] == 0) break; // skip padding
            //Rcout << "Join " << tokens[start] << " at " << start << " with " << next << "\n";
            skip(tokens, tokens_ng, set_words, next, n, skips, ngram, map_ngram, id_ngram);
        }
    } else {
        if (set_words.size() > 0) { // for compounding
            auto it = set_words.find(ngram);
            if (it != set_words.end()) {
                //tokens_ng.push_back(ngram_id(ngram, map_ngram, id_ngram));
            } else {
                Ngram unigram(1, tokens[start]);
                tokens_ng.push_back(ngram_id(unigram, map_ngram, id_ngram));
            }
        } else {
            tokens_ng.push_back(ngram_id(ngram, map_ngram, id_ngram));
        }
    }
}