#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cSwap (const SEXP val1, const SEXP val2, List coll) {

	const int coll_size  = coll.size();
	const int flags      = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return coll;
	} else {

		List out = clone(coll);

		for (int ith = 0; ith < coll_size; ++ith) {

			bool is_match = R_compute_identical(coll[ith], val1, flags);

			if (is_match) {
				out[ith] = val2;
			}
		}

		return out;

	}

}
