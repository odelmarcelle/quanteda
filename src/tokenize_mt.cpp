//#include "dev.h"
#include "lib.h"
#include <stringi.cpp>
using namespace quanteda;


#include <stringi.h>
#include <Rcpp.h>
using namespace Rcpp;


void tokenize(const Corpus &texts, 
              //List &tokens_, 
              std::size_t begin, 
              std::size_t end
              //,
              //const SEXP &n_, 
              //const SEXP &tokens_only_, 
              //const SEXP &simplify_, 
              //const SEXP &opts_
              ){
    
    Corpus texts_sub;
    //texts_sub.reserve(end - begin);
    for (std::size_t h = begin; h < end; h++) {
        texts_sub.push_back(texts[h]);
    }
    
    CharacterVector texts_sub_ = Rcpp::wrap(texts_sub);
    IntegerVector n_ = {-1};
    LogicalVector tokens_only_ = {0};
    LogicalVector simplify_ = {0};
    List opts_ = List::create(Named("type") = "word");
    
    List temp_ = stri_split_boundaries(texts_sub_, n_, tokens_only_, simplify_, opts_);
    // std::size_t i = 0;
    // for (std::size_t h = begin; h < end; h++) {
    //     //tokens_[h] = temp_[i++];
    // }
}

struct tokenize_mt : public Worker{
    
    const Corpus &texts;
    //List &tokens_; 
    
    // Constructor
    // tokenize_mt(const Corpus &texts_, List &tokens__):
    //             texts(texts_), tokens_(tokens__) {}
    // 
    tokenize_mt(const Corpus &texts_):
                texts(texts_) {}
     
    void operator()(std::size_t begin, std::size_t end){
        //Rcout << "Range " << begin << " " << end << "\n";
        //tokenize(texts_, tokens_, begin, end, n_, tokens_only_, simplify_, opts_);
        tokenize(texts, begin, end);
    }
};


//' @export
// [[Rcpp::export]]
List qatd_cpp_tokenize(const CharacterVector texts_) {
    Corpus texts = Rcpp::as<Corpus>(texts_);
    // dev::Timer timer;
    //StringTexts temp(texts.size());
    List tokens_(texts.size());
    
    // dev::start_timer("Dictionary detect", timer);
#if QUANTEDA_USE_TBB
   //tokenize_mt tokenize_mt(texts_, tokens_, n_, tokens_only_, simplify_, opts_);
   tokenize_mt tokenize_mt(texts);
   parallelFor(0, texts.size(), tokenize_mt);
#else
    //tokenize(texts_, tokens_, 0, texts_.size(), n_, tokens_only_, simplify_, opts_);
    tokenize(texts, 0, texts_.size());
#endif
    return(tokens_);
}



/***R
opt <- stringi::stri_opts_brkiter(type = "word")
txt <- c("aa bb cc", "dd aa ee")
RcppParallel::setThreadOptions(2)
microbenchmark::microbenchmark(
out <- qatd_cpp_tokenize(rep(txt, 10))
)
*/
