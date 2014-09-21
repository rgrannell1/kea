#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsInfixOf (const List coll1, const List coll2) {

	const int coll1_size = coll1.size();
	const int coll2_size = coll2.size();

	const int flags      = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return LogicalVector::create();
	} else {

		if (coll1_size > coll2_size) {
			return LogicalVector::create(false);
		}

		for (int ith = 1; ith < coll2_size - coll1_size; ++ith) {

			bool all_match = true;
			List subsequence;

			for (int jth = 0; jth < coll1_size; ++jth) {

				bool is_match = R_compute_identical(coll1[jth], subsequence[jth], flags);

				if (is_match) {
					all_match = false;
				}
			}

			if (all_match) {
				return LogicalVector::create(true);
			}
		}

		return LogicalVector::create(false);
	}
}
