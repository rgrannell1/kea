#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cUnzipIndices (const List& coll) {

	const R_len_t coll_len   = coll.size();
	const bool has_names = coll.attr("names") != R_NilValue;

	if (coll_len == 0) {

		List out(0);

		if (has_names) {
			out.attr("names") = CharacterVector::create();
		}

		return out;

	} else {

		List out(coll_len);

		if (has_names) {

			for (R_len_t ith = 0; ith < coll_len; ++ith) {
				out[ith] = List::create( ith + 1, coll[ith] );
			}

			out.attr("names") = (CharacterVector)coll.attr("names");

		} else {

			for (R_len_t ith = 0; ith < coll_len; ++ith) {
				out[ith] = List::create(ith + 1, coll[ith]);
			}
		}

		return out;
	}
}
