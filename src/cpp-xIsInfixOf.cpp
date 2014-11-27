#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsInfixOf (const List& coll1, const List& coll2) {

	const R_len_t coll1_size = coll1.size();
	const R_len_t coll2_size = coll2.size();

	const R_len_t flags      = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return LogicalVector::create();
	} else {

		if (coll1_size > coll2_size) {
			return LogicalVector::create(false);
		}

		for (R_len_t lower = 0; lower < (coll2_size - coll1_size) + 1; ++lower) {

			bool subseq_match = true;

			for (R_len_t ith = 0; ith < coll1_size; ++ith) {
				if (!(bool) R_compute_identical(coll1[ith], coll2[ith + lower], flags)) {

					subseq_match = false;
					break;

				}
			}

			if (subseq_match) {
				return LogicalVector::create(true);
			}

		}

		return LogicalVector::create(false);

	}
}
