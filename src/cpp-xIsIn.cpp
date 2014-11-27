#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsIn (SEXP val, const List& coll) {

	R_len_t coll_len = coll.size();
	const R_len_t flags = 1 + 2 + 4 + 8 + 0;

	if (coll_len == 0) {
		return LogicalVector::create();
	} else {

		for (R_len_t ith = 0; ith < coll_len; ++ith) {
			if ((bool)R_compute_identical(val, coll[ith], flags)) {
				// required due to RcppCore/Rcpp issue #177.
				return LogicalVector::create(true);
			}
		}

		return LogicalVector::create(false);
	}
}
