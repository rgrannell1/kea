#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cUnzipKeys (const List& coll) {

	const int coll_len = coll.size();

	if (coll_len == 0) {
		return List::create();
	} else {

		List out(coll_len);
		CharacterVector coll_names = coll.attr("names");

		for (int ith = 0; ith < coll_len; ++ith) {

			String key = coll_names[ith];
			List row   = List::create(key, coll[ith]);
			out[ith]   = row;
		}

		return out;
	}
}
