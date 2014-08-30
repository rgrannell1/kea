#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cChunk (NumericVector num, List coll) {

	int num_len    = num.size();
	int coll_len   = coll.size();

	bool has_names = coll.attr("names") != R_NilValue;

	if (coll_len == 0 || num_len == 0) {
		return List::create();
	} else {

		for (int lower = 0; lower < coll_len; lower += num[0]) {

		}

	}

}
