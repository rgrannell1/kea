#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cTake (NumericVector num, List coll) {

	const int num_len    = num.size();
	const int coll_len   = coll.size();

	const bool has_names = coll.attr("names") != R_NilValue;

	if (coll_len == 0 || num_len == 0 || num[0] == 0) {

		List out(0);

		if (has_names) {
			out.attr("names") = CharacterVector::create();
		}

		return out;

	} else {

		int upper = num[0] > coll_len ? coll_len: num[0];

		if (has_names) {

			CharacterVector coll_names = coll.attr("names");

			List out          = head(coll, upper);
			out.attr("names") = head(coll_names, upper);

			return out;

		} else {

			return head(coll, upper);

		}
	}
}
