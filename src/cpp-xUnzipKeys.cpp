#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cUnzipKeys (const List& coll) {

	const R_len_t coll_len = coll.size();

	if (coll_len == 0) {
		return List::create();
	} else {

		List out(coll_len);
		CharacterVector coll_names = coll.attr("names");

		for (R_len_t ith = 0; ith < coll_len; ++ith) {
			out[ith] = List::create( (String)coll_names[ith], coll[ith] );
		}

		return out;
	}
}
