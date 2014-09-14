#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cIsIn (SEXP val, List coll) {

	int coll_len = coll.size();

	if (coll_len == 0) {
		return LogicalVector::create();
	} else {

		Function identical("identical");

		for (int ith = 0; ith < coll_len; ++ith) {

			SEXP elem = coll[ith];

			//use R_compute_identical instead!!!
			Shield<SEXP> result(identical(val, elem));
			if (LOGICAL(result)[0]) {
				return LogicalVector::create(true);
			}
		}

		return LogicalVector::create(false);
	}
}
