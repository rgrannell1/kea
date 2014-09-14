#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector cIsIn (SEXP val, List coll) {

	int coll_len = coll.size();

	if (coll_len == 0) {
		return LogicalVector::create();
	} else {

		for (int ith = 0; ith < coll_len; ++ith) {

			SEXP elem   = coll[ith];
			bool result = R_compute_identical(val, elem, 0);

			if (result) {
				// required due to RcppCore/Rcpp issue #177.
				return LogicalVector::create(true);
			}
		}

		return LogicalVector::create(false);
	}
}
