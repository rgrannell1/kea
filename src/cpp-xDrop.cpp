#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
List cDrop (NumericVector num, List coll) {

	const int num_len    = num.size();
	const int coll_len   = coll.size();

	const bool has_names = coll.attr("names") != R_NilValue;

	if (coll_len == 0 || num_len == 0 || num[0] >= coll_len) {

		List out(0);

		if (has_names) {
			out.attr("names") = CharacterVector::create();
		}

		return out;

	} else {

		if (has_names) {

			CharacterVector coll_names = coll.attr("names");

			List out          = tail(coll,       coll_len - num[0]);
			out.attr("names") = tail(coll_names, coll_len - num[0]);

			return out;

		} else {

			return tail(coll, coll_len - num[0]);

		}
	}

}
