#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsSubsetOf (const List& coll1, const List& coll2) {

	const int coll1_size = coll1.size();
	const int coll2_size = coll2.size();

	const int flags = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return LogicalVector::create();
	} else {

		for (int ith = 0; ith < coll1_size; ++ith) {

			bool has_match = false;

			for (int jth = 0; jth < coll2_size; ++jth) {
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
