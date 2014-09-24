#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;






// [[Rcpp::export]]
IntegerVector cPoll (const Function& pred, const List& coll) {

	const int coll_size = coll.size();

	if (coll_size == 0) {
		return IntegerVector::create();
	} else {

		int count = 0;

		for (int ith = 0; ith < coll_size; ++ith) {

			Shield<SEXP> is_match( pred(coll[ith]) );

			Must_Be_Flag("pred", is_match);

			if (is_match) {
				++count;
			}
		}

		return IntegerVector::create(count);
	}
}
