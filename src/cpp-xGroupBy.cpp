#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;





// [[Rcpp::export]]
List cGroupBy (Function fn, List coll) {

	int coll_size   = coll.size();
	const int flags = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return List::create();
	} else {

		std::vector< std::vector<int> > group_members;
		std::vector<SEXP> group_maps;

		for (int ith = 0; ith < coll_size; ++ith) {

			bool group_found = false;

			Shield<SEXP> map( fn(coll[ith]) );

			int group_maps_size = group_maps.size();

			for (int jth = 0; jth < group_maps_size; ++jth) {

				bool is_match = R_compute_identical(map, group_maps[jth], flags);

				if (is_match) {

					std::vector<int> group_indices = group_members[jth];
					group_indices.push_back(ith);
					group_members[jth] = group_indices;

					group_found = true;
				}
			}

			if (!group_found) {

				std::vector<int> group_indices;
				group_indices.push_back(ith);

				group_members.push_back(group_indices);
				group_maps.push_back(map);
			}
		}

		int group_maps_size = group_maps.size();
		List out(group_maps_size);

		for (int ith = 0; ith < group_maps_size; ++ith) {

			std::vector<int> group_indices = group_members[ith];
			int group_indices_size         = group_indices.size();

			List elements(group_indices_size);

			for (int jth = 0; jth < group_indices_size; ++jth) {

				int index     = group_indices[jth];
				elements[jth] = coll[index];

			}

			List group = List::create(group_maps[ith], elements);
			out[ith]   = group;
		}

		return out;

	}
}
