#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
List cGather (const List coll) {

	R_len_t coll_size   = coll.size();
	const R_len_t flags = 1 + 2 + 4 + 8 + 0;

	std::vector<int> lower_bounds;
	std::vector<int> upper_bounds;

	lower_bounds.push_back(0);

	for (R_len_t ith = 1; ith < coll_size; ++ith) {
		if (!(bool) R_compute_identical(coll[ith - 1], coll[ith], flags)) {
			if (ith != 0) {
				upper_bounds.push_back(ith - 1);
			}

			lower_bounds.push_back(ith);
		}
	}

	upper_bounds.push_back(coll_size - 1);

	const R_len_t lower_bounds_size = lower_bounds.size();

	List out(lower_bounds_size);

	const bool has_names = coll.attr("names") != R_NilValue;

	if (has_names) {

		R_len_t name_counter           = 0;
		CharacterVector coll_names = coll.attr("names");

		for (R_len_t ith = 0; ith < lower_bounds_size; ++ith) {

			R_len_t interval = upper_bounds[ith] - lower_bounds[ith] + 1;

			List group(interval);
			CharacterVector group_names(interval);

			for (R_len_t jth = 0; jth < interval; ++jth) {

				group[jth]       = coll[lower_bounds[ith]];
				group_names[jth] = coll_names[name_counter];
				++name_counter;

			}

			group.attr("names") = group_names;
			out[ith]            = group;
		}

	} else {

		for (R_len_t ith = 0; ith < lower_bounds_size; ++ith) {

			R_len_t interval = upper_bounds[ith] - lower_bounds[ith] + 1;
			List group(interval);

			for (R_len_t jth = 0; jth < interval; ++jth) {
				group[jth] = coll[lower_bounds[ith]];
			}

			out[ith] = group;
		}

	}

	return out;
}
