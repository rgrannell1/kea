#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cUnzipKeys (List coll) {

	int coll_len               = coll.size();
	CharacterVector coll_names = coll.attr("names");

	if (coll_len == 0) {

		List out(0);
		out.attr("names") = CharacterVector::create();

		return out;

	} else {

		List out(coll_len);

		for (int ith = 0; ith < coll_len; ith++) {

			String key = coll_names[ith];
			List row   = List::create(key, coll[ith]);
			out[ith]   = row;
		}

		return out;
	}
}
