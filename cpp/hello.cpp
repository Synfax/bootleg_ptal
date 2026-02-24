#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sum_vector(IntegerVector vector) {

  int vector_length = vector.size();

  int vector_sum = 0;

  for(int i = 0; i < vector_length; i++) {

    vector_sum = vector_sum + vector[i];

  }

  return(vector_sum);

}
