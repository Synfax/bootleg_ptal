#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector more_than_elapsed(NumericVector vector, double elapsed_time) {

  std::vector<int> matches;

  for(int i = 0; i < vector.size(); i++) {

    int current_value = vector[i];

    if(current_value >= elapsed_time) {

      matches.push_back(i);

    }
  }

  return(Rcpp::wrap(matches));
}
