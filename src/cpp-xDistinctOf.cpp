#include <Rcpp.h>
#import <math.h>
#include "functions.h"
using namespace Rcpp;



// NOT CURRENTLY USING!! TOO SLOW!!
// seems to have worse time complexity than  builtin.

// [[Rcpp::export]]
List cDistinctOf (const List& coll) {

	const R_len_t coll_size = coll.size();
	const R_len_t flags     = 1 + 2 + 4 + 8 + 0;

	std::vector<SEXP> unique;
	unique.push_back(coll[0]);

	for (R_len_t ith = 1; ith < coll_size; ++ith) {

		bool match_found = false;
		R_len_t unique_size  = unique.size();

		for (R_len_t jth = 0; jth < unique_size; ++jth) {
			if ((bool)R_compute_identical(coll[ith], unique[jth], flags)) {
				match_found = true;
				break;
			}
		}

		if (!match_found) {
			unique.push_back(coll[ith]);
		}
	}

	return wrap(unique);
}
