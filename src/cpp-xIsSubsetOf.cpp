#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsSubsetOf (const List& coll1, const List& coll2) {

	const R_len_t coll1_size = coll1.size();
	const R_len_t coll2_size = coll2.size();

	const R_len_t flags = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return LogicalVector::create();
	} else {

		for (R_len_t ith = 0; ith < coll1_size; ++ith) {

			bool has_match = false;

			for (R_len_t jth = 0; jth < coll2_size; ++jth) {
				if ((bool) R_compute_identical(coll1[ith], coll2[jth], flags)) {
					has_match = true;
				}
			}

			if (!has_match) {
				return LogicalVector::create(false);
			}

		}

		return LogicalVector::create(true);
	}
}
