#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cSwap (const SEXP val1, const SEXP val2, const List& coll) {

	const R_len_t coll_size  = coll.size();
	const R_len_t flags      = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return coll;
	} else {

		List out = clone(coll);

		for (R_len_t ith = 0; ith < coll_size; ++ith) {
			if ((bool) R_compute_identical(coll[ith], val1, flags)) {
				out[ith] = val2;
			}
		}

		return out;
	}

}
