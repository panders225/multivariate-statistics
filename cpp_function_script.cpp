#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}


// [[Rcpp::export]]
int one() {
  return 1;
}

// [[Rcpp::export]]
int signC(int x) {
  if (x > 0) {
    return 1;
  }
  else if (x == 0) {
    return 0;
  }
  else {
    return -1;
  }
}

/*** R
signC(-34)
*/


// [[Rcpp::export]]
double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; i++) {
    total += x[i];
  }
  return total;
}


