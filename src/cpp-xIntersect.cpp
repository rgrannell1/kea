#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
List cIntersect (const List& coll1, const List& coll2) {

	const int coll1_size = coll1.size();
	const int coll2_size = coll2.size();

	const int flags = 1 + 2 + 4 + 8 + 0;

	if (coll1_size == 0 || coll2_size == 0) {
		return List::create();
	} else {

		std::vector<int> shared_indices;

		for (int ith = 0; ith < coll1_size; ++ith) {

			bool has_match = false;

			for (int jth = 0; jth < coll2_size; ++jth) {

				bool is_match = R_compute_identical(coll1[ith], coll2[jth], flags);

				if (is_match) {
					has_match = true;
					break;
				}
			}

			if (has_match) {
				shared_indices.push_back(ith);
			}
		}

		const int shared_indices_size = shared_indices.size();
		List out(shared_indices_size);

		for (int ith = 0; ith < shared_indices_size; ++ith) {
			out[ith]         = coll1[ (shared_indices[ith]) ];
		}

		return out;
	}
}
