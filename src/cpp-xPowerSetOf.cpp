#include <Rcpp.h>
#import <math.h>
using namespace Rcpp;

List concat (List coll0, List coll1) {

	List out (coll0.size() + coll1.size());

	int ith = 0;

	for (int jth = 0; jth < coll0.size(); ++jth) {
		out[ith] = coll0[jth];
		++ith;
	}

	for (int jth = 0; jth < coll1.size(); ++jth) {
		out[ith] = coll1[jth];
		++ith;
	}

	return out;
}





// [[Rcpp::export]]
List cPowerSetOf (List coll) {

	int coll_len = coll.size();

	if (coll_len == 0) {
		return List::create();
	} else {

		List subsets (1);
		subsets[0] = List::create();

		for (int ith = 0; ith < coll_len; ++ith) {

			int subsets_len = subsets.size();
			List elem_subsets (subsets_len);

			for (int jth = 0; jth < subsets_len; ++jth) {
				elem_subsets[jth] = concat(List::create(coll[ith]), subsets[jth]);
			}

			subsets = concat(subsets, elem_subsets);
		}

		return subsets;
	}
}
