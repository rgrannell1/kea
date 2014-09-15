#include <Rcpp.h>
using namespace Rcpp;

// Worst case should be O(n^2), when there are n distinct elements.



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

		while (not_found.size() > 0) {

			int count       = 1;

			int first_index = not_found[0];

			std::vector<int> to_remove;

			for (int jth = 1; jth < not_found.size(); ++jth) {

				int second_index = not_found[jth];

				if (std::find(not_found.begin(), not_found.end(), second_index) != not_found.end()) {

					bool is_match = R_compute_identical(coll[first_index], coll[second_index], flags);

					if (is_match) {
						to_remove.push_back(jth);
						++count;
					}
				}

			}

			for (int jth = 1; jth < to_remove.size(); ++jth) {
				not_found.erase(not_found.begin() + to_remove[jth]);
			}

			not_found.erase(not_found.begin());

			groups.push_back(first_index);
			groups.push_back(count);
		}

		//int groups_size = groups.size();

		//int out_ith = 0;
		//List out(groups_size / 2);

		//for (int ith = 0; ith < groups_size; ith += 2) {

		//	int elem_ith   = groups[ith];
		//	int elem_count = groups[ith + 1];

		//	List outgroup  = List::create(coll[elem_ith], elem_count);

		//	out[out_ith]   = outgroup;
		//	++out_ith;
		//}

		//return out;

	}

}
