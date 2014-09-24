#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cNotInfixOf (const List& coll1, const List& coll2) {

	const int coll1_size = coll1.size();
	const int coll2_size = coll2.size();

	const int flags      = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return LogicalVector::create();
	} else {

		if (coll1_size > coll2_size) {
			return LogicalVector::create(true);
		}

		for (int lower = 0; lower < (coll2_size - coll1_size) + 1; ++lower) {

			bool subseq_match = true;

			for (int ith = 0; ith < coll1_size; ++ith) {

				bool is_match = R_compute_identical(coll1[ith], coll2[ith + lower], flags);

				if (!is_match) {

					subseq_match = false;
					break;

				}
			}

			if (subseq_match) {
				return LogicalVector::create(false);
			}

		}

		return LogicalVector::create(true);

	}
}
