#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;

// Worst case should be O(n^2), when there are n distinct elements.





// [[Rcpp::export]]
List cTabulate (List coll) {

	const int coll_size = coll.size();
	const int flags     = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return List::create();
	}

	std::vector<int> unique_indices;
	std::vector<int> unique_counts;

	std::vector<int> unbinned_indices = indices_to(coll_size);

	while (unbinned_indices.size() > 0) {

		std::vector<int> just_binned;

		int copies = 1;
		int unique_index = unbinned_indices[0];

		unique_indices.push_back(unique_index);
		just_binned.push_back(unique_index);

		for (int ith = 1; ith < unbinned_indices.size(); ++ith) {

			int candidate_index = unbinned_indices[ith];
			bool is_match       = R_compute_identical(coll[unique_index], coll[candidate_index], flags);

			if (is_match) {
				just_binned.push_back(candidate_index);
				++copies;
			}

		}

		unique_counts.push_back(copies);

		// filter the recently binned indices out of the unbinned indices.
		std::vector<int> tmp;

		// is this really optimally efficient?
		for (int ith = 0; ith < unbinned_indices.size(); ++ith) {

			bool still_unbinned = true;

			for (int jth = 0; jth < just_binned.size(); ++jth) {
				if (unbinned_indices[ith] == just_binned[jth]) {

					still_unbinned = false;
					break;

				}
			}

			if (still_unbinned) {
				tmp.push_back(unbinned_indices[ith]);
			}
		}

		unbinned_indices = tmp;
	}

	List out(unique_indices.size());

	for (int ith = 0; ith < unique_indices.size(); ++ith) {

		int unique_index = unique_indices[ith];
		int copies       = unique_counts[ith];

		List pair        = List::create(coll[unique_index], copies);
		out[ith]         = pair;
	}

	return out;
}
