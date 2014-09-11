#include <Rcpp.h>
using namespace Rcpp;


void Must_Be_Of_Length (const std::string COLL, const List coll, const NumericVector lengths) {

	const int coll_size    = coll.size();
	const int lengths_size = lengths.size();

	bool match_found = false;

	for (int ith = 0; ith < lengths_size; ++ith) {
		if (coll_size == lengths[ith]) {
			match_found = true;
		}
	}

	if (!match_found) {

		const std::string message = "The argument matching " + COLL + " must have length in the set {" + "}.\n" + \
		"The actual length was " + ".";

		stop(message);
	}

}


// [[Rcpp::export]]
List cZipKeys (const List colls) {

	const int colls_size = colls.size();

	if (colls_size == 0) {
		return List::create();
	} else {

		List out (colls_size);
		CharacterVector keys(colls_size);

		for (int ith = 0; ith < colls_size; ++ith) {

			List coll  = colls[ith];

			Must_Be_Of_Length("key", coll[0], 1);

			String key = coll[0];

			out[ith]   = coll[1];
			keys[ith]  = key;

 		}

		out.attr("names") = keys;

		return out;
	}

}
