#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsIn (SEXP val, List coll) {

	int coll_len = coll.size();
	const int flags = 1 + 2 + 4 + 8 + 0;

	if (coll_len == 0) {
		return LogicalVector::create();
	} else {

		for (int ith = 0; ith < coll_len; ++ith) {

			bool result = R_compute_identical(val, coll[ith], flags);

			if (result) {
				// required due to RcppCore/Rcpp issue #177.
				return LogicalVector::create(true);
			}
		}

		return LogicalVector::create(false);
	}
}
