#include <Rcpp.h>
#import <math.h>
#include "functions.h"
using namespace Rcpp;



// NOT CURRENTLY USING!! TOO SLOW!!


// [[Rcpp::export]]
List cUniqueOf (const List& coll) {

	const int coll_size = coll.size();
	const int flags     = 1 + 2 + 4 + 8 + 0;

	std::vector<SEXP> unique;
	unique.push_back(coll[0]);

	for (int ith = 1; ith < coll_size; ++ith) {

		bool match_found = false;

		for (int jth = 0; jth < unique.size(); ++jth) {

			bool is_match = R_compute_identical(coll[ith], unique[jth], flags);

			if (is_match) {
				match_found = true;
			}

		}

		if (!match_found) {
			unique.push_back(coll[ith]);
		}
	}

	return wrap(unique);
}
