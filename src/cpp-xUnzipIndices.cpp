#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cUnzipIndices (const List& coll) {

	const int coll_len   = coll.size();
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

			for (int ith = 0; ith < coll_len; ++ith) {

				List row = List::create(ith + 1, coll[ith]);
				out[ith] = row;
			}

			CharacterVector coll_names = coll.attr("names");
			out.attr("names")          = coll_names;

		} else {

			for (int ith = 0; ith < coll_len; ++ith) {

				List row = List::create(ith + 1, coll[ith]);
				out[ith] = row;
			}
		}

		return out;
	}
}
