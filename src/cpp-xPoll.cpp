#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;






// [[Rcpp::export]]
IntegerVector cPoll (const Function pred, const List coll) {

	const int coll_size = coll.size();

	if (coll_size == 0) {
		return IntegerVector::create();
	} else {

		int count = 0;

		for (int ith = 0; ith < coll_size; ++ith) {

			bool is_match( pred(coll[ith]) );

			if (is_match) {
				count++;
			}

		}

		return count;
	}
}
