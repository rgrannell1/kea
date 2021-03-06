#include <Rcpp.h>
#import <math.h>
#include "functions.h"
using namespace Rcpp;





// [[Rcpp::export]]
List cPowerSetOf (const List& coll) {

	const R_len_t coll_len = coll.size();

	if (coll_len == 0) {
		return List::create();
	} else {

		std::vector<List> subsets;
		subsets.push_back(List::create());

		for (R_len_t ith = 0; ith < coll_len; ++ith) {

			R_len_t subsets_len = subsets.size();

			for (R_len_t jth = 0; jth < subsets_len; ++jth) {
				subsets.push_back( concat(List::create(coll[ith]), subsets[jth]) );
			}

		}

		return wrap(subsets);
	}
}
