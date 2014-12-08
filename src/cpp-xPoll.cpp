#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;






// [[Rcpp::export]]
IntegerVector cPoll (const Function& pred, const List& coll) {

	const R_len_t coll_size = coll.size();

	if (coll_size == 0) {
		return IntegerVector::create();
	} else {

		R_len_t count = 0;

		for (R_len_t ith = 0; ith < coll_size; ++ith) {

			Shield<SEXP> is_match( pred(coll[ith]) );

			Must_Be_Flag("pred", is_match);

			if (is_match) {
				++count;
			}
		}

		return IntegerVector::create(count);
	}
}
