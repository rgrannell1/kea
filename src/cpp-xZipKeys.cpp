#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cZipKeys (List colls) {

	const int colls_size = colls.size();

	if (colls_size == 0) {
		return List::create();
	} else {

		List out (colls_size);
		CharacterVector keys(colls_size);

		for (int ith = 0; ith < colls_size; ++ith) {

			List coll  = colls[ith];
			String key = coll[0];

			//MACRO( Must_Be_Of_Length(key, 1) )
			//TODO TODO TODO

			out[ith]   = coll[1];
			keys[ith]  = key;

 		}

		out.attr("names") = keys;

		return out;
	}

}
