#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;

// Worst case should be O(n^2), when there are n distinct elements.





// [[Rcpp::export]]
List cTabulate (const List& coll) {

	const int coll_size = coll.size();
	const int flags     = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return List::create();
	}

	std::vector<int> unique_indices;
	std::vector<int> unique_counts;

	std::vector<int> unbinned_indices;

	for (int ith = 0; ith < coll_size; ++ith) {
		unbinned_indices.push_back(ith);
	}

	while (unbinned_indices.size() > 0) {

		std::vector<int> just_binned;

		int frequency    = 1;
		int unique_index = unbinned_indices[0];

		just_binned   .push_back(unique_index);
		unique_indices.push_back(unique_index);

		int unbinned_indices_size = unbinned_indices.size();

		for (int ith = 1; ith < unbinned_indices_size; ++ith) {
			// find which unbinned indices point to the same value as the current unique value.

			int candidate_index = unbinned_indices[ith];

			if ((bool) R_compute_identical(coll[unique_index], coll[candidate_index], flags)) {
				just_binned.push_back(candidate_index);
				++frequency;
			}

		}

		unique_counts.push_back(frequency);

		// filter the recently binned indices out of the unbinned indices.
		std::vector<int> tmp;

		// is this really optimally efficient? a binary tree might be more successful.
		int unique_indices_size = unbinned_indices.size();

		for (int ith = 0; ith < unique_indices_size; ++ith) {

			bool still_unbinned = true;
			int just_binned_size = just_binned.size();

			for (int jth = 0; jth < just_binned_size; ++jth) {
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

	const int unique_indices_size = unique_indices.size();
	List out(unique_indices_size);

	for (int ith = 0; ith < unique_indices_size; ++ith) {
		out[ith] = List::create( coll[unique_indices[ith]], unique_counts[ith] );
	}

	return out;
}
