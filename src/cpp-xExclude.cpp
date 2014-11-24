#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
List cExclude (const List& coll1, const List& coll2) {

	const int coll1_size = coll1.size();
	const int coll2_size = coll2.size();

	const int flags      = 1 + 2 + 4 + 8 + 0;
	const bool has_names = coll2.attr("names") != R_NilValue;

	if (coll1_size == 0) {
		return coll2;
	} else if (coll2_size == 0) {
		return List::create();
	} else {

		std::vector<int> remaining_indices;

		for (int ith = 0; ith < coll2_size; ++ith) {

			bool has_match = false;

			for (int jth = 0; jth < coll1_size; ++jth) {

				if ((bool) R_compute_identical(coll2[ith], coll1[jth], flags)) {
					has_match = true;
					break;
				}

			}

			if (!has_match) {
				remaining_indices.push_back(ith);
			}
		}

		const int remaining_indices_size = remaining_indices.size();
		List out(remaining_indices_size);

		if (has_names) {

			CharacterVector coll_names = coll2.attr("names");
			CharacterVector out_names(remaining_indices_size);

			for (int ith = 0; ith < remaining_indices_size; ++ith) {

				out[ith]       = coll2[ remaining_indices[ith] ];
				out_names[ith] = coll_names[ remaining_indices[ith] ];

			}

			out.attr("names") = out_names;

		} else {

			for (int ith = 0; ith < remaining_indices_size; ++ith) {
				out[ith] = coll2[ remaining_indices[ith] ];
			}

		}

		return out;

	}
}
