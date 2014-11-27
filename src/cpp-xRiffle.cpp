#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cRiffle (SEXP val, const List& coll) {

	const R_len_t coll_len = coll.size();

	if (coll_len == 0) {
		return List::create();
	} else if (coll_len == 1) {
		return List::create(coll[0]);
	} else {

		List out ((2 * coll_len) - 1);

		out[0]  = coll[0];
		R_len_t jth = 1;

		for (R_len_t ith = 1; ith < coll_len; ++ith) {

			out[jth]     = val;
			out[jth + 1] = coll[ith];

			jth += 2;

		}

		return out;
	}
}
