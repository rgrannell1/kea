
#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;





// [[Rcpp::export]]
List cAmassBy (const Function fn, const List& coll) {

	const R_len_t coll_size   = coll.size();
	const R_len_t flags = 1 + 2 + 4 + 8 + 0;

	if (coll_size == 0) {
		return List::create();
	} else {

		std::vector< std::vector<R_len_t> > group_members;
		std::vector<SEXP> group_maps;

		for (R_len_t ith = 0; ith < coll_size; ++ith) {

			bool group_found = false;

			Shield<SEXP> map( fn(coll[ith]) );

			R_len_t group_maps_size = group_maps.size();

			for (R_len_t jth = 0; jth < group_maps_size; ++jth) {
				if ((bool) R_compute_identical(map, group_maps[jth], flags)) {

					std::vector<R_len_t> group_indices = group_members[jth];
					group_indices.push_back(ith);
					group_members[jth]             = group_indices;

					group_found = true;
				}
			}

			if (!group_found) {

				std::vector<R_len_t> group_indices;
				group_indices.push_back(ith);

				group_members.push_back(group_indices);
				group_maps.push_back(map);
			}
		}

		R_len_t group_maps_size = group_maps.size();
		List out(group_maps_size);

		for (R_len_t ith = 0; ith < group_maps_size; ++ith) {

			std::vector<R_len_t> group_indices = group_members[ith];
			R_len_t group_indices_size         = group_indices.size();

			List elements(group_indices_size);

			for (R_len_t jth = 0; jth < group_indices_size; ++jth) {
				elements[jth] = coll[ (group_indices[jth]) ];
			}

			List group = elements;
			out[ith]   = group;
		}

		return out;

	}
}
