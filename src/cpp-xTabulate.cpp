#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
List cTabulate (List coll) {

	const int coll_size = coll.size();
	const int flags     = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return List::create();
	} else {

		std::vector<int> groups;
		std::vector<int> not_found;

		for (int ith = 0; ith < coll_size; ith++) {
			not_found.push_back(ith);
		}

		for (int ith = 0; ith < coll_size; ++ith) {

			if (not_found[ith] == ith) {
				// match not found for ith element.

				SEXP elem = coll[ith];

				int count      = 1;
				not_found[ith] = -1;

				for (int jth = 0; jth < coll_size; ++jth) {

					if (not_found[jth] == jth) {
						// match not found for jth element.

						bool is_match = R_compute_identical(elem, coll[jth], flags);

						if (is_match); {
							count++;
						}
					}

				}

				groups.push_back(ith);
				groups.push_back(count);

			}
		}

		int groups_size = groups.size();

		int out_ith = 0;
		List out(groups_size / 2);

		for (int ith = 0; ith < groups_size; ith += 2) {

			int elem_ith   = groups[ith];
			int elem_count = groups[ith + 1];

			List outgroup  = List::create(coll[elem_ith], elem_count);

			out[out_ith]   = outgroup;
			++out_ith;
		}

		return out;

	}

}
