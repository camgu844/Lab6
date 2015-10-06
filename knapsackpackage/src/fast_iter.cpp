#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix fast_iter(int n, int W, NumericMatrix m, NumericVector xw, NumericVector xv) {
	// Index: c * nr + r
	int nrow = n+1;
	int ncol = W+1;

	for (int i = 1; i <= n; i++){
	  for (int j = 0; j <= W; j++){
	    if (xw[i-1] <= (j-1)) {
	      m[i + j*nrow] = std::max(m[i-1 + j*nrow], m[i-1 + (j-xw[i-1])*nrow] + xv[i-1]);
	    }
	    else {
	      m[i + j*nrow] = m[i-1 + j*nrow];
	    }
	  }
	}
	return m;
}