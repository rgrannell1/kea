#include <Rcpp.h>
#import <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cPowerSetOf (List coll) {

	int coll_len   = coll.size();

	if (coll_len == 0) {
		return List::create();
	} else {

		List subsets(pow(2, coll_len) + 1);
		subsets[0] = List::create();

		int elem_index = 1;

		for (int ith = 0; ith < coll_len; ith++) {

			for (int jth = 0; jth < elem_index; jth++) {

				subsets[elem_index] = subsets[jth];//, elem;
				elem_index++;

			}

		}

		return subsets;
	}
}
