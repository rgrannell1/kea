#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
LogicalVector cIsPrefixOf (const List coll1, const List coll2) {

	const int coll1_size = coll1.size();
	const int coll2_size = coll2.size();

	const int flags     = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return LogicalVector::create();
	} else{

		if (coll1_size > coll2_size) {
			return LogicalVector::create(false);
		}

		for (int ith = 0; ith < coll1_size; ++ith) {

			bool is_match = R_compute_identical(coll1[ith], coll2[ith], flags);

			if (is_match) {
				return LogicalVector::create(false);
			}

		}

		return LogicalVector::create(true);
	}
}
