#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cPoll (Function pred, List coll) {

	int coll_len = coll.size();

	if (coll_len == 0) {
		return NumericVector::create();
	} else {

		int count = 0;

		for (int ith = 0; ith < coll_len; ith++) {

			LogicalVector is_match = pred(coll[ith]);

			if (is_match[0] == true) {
				count++;
			}
		}

		return NumericVector::create(count);
	}
}
