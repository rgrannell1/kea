#include <Rcpp.h>
#import <math.h>
#include "functions.h"
using namespace Rcpp;




// [[Rcpp::export]]
List cUniqueOf (const List coll) {

	const int coll_size = coll.size();
	const int flags     = 1 + 2 + 4 + 8 + 0;

	std::vector<int> which_unique;
	which_unique.push_back(0);

	for (int ith = 0; ith < coll_size; ++ith) {

		bool match_found = false;

		for (int jth = 0; jth < which_unique.size(); ++jth) {

			int index = which_unique[jth];
			bool is_match = R_compute_identical(coll[ith], coll[index], flags);

			if (is_match) {
				match_found = true;
			}
		}

		if (!match_found) {
			which_unique.push_back(ith);
		}
	}

	const int which_unique_size = which_unique.size();

	List set(which_unique_size);

	for (int ith = 0; ith < which_unique_size; ++ith) {
		set[ith] = coll[which_unique[ith]];
	}

	return wrap(set);
}
