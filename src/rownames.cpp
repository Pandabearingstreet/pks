/*
 * calculates row names for a logical matrix as the pattern of the matrix. equivalent, but faster than as.pattern()
 */
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector concat_rows(LogicalMatrix mat) {
  int nrow = mat.nrow(), ncol = mat.ncol();
  CharacterVector out(nrow);

  for (int i = 0; i < nrow; ++i) {
    std::string s = "";
    for (int j = 0; j < ncol; ++j) {
      s += mat(i, j) ? "1" : "0";
    }
    out[i] = s;
  }

  return out;
}
