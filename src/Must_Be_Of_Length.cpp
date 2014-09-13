#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;





void Must_Be_Of_Length (const std::string COLL, const List coll, const List lengths) {

	const int coll_size    = coll.size();
	const int lengths_size = lengths.size();

	bool match_found       = false;

	for (int ith = 0; ith < lengths_size; ++ith) {
		if (coll_size == lengths[ith]) {
			match_found = true;
		}
	}

	if (!match_found) {

		const std::string message = "\nThe argument matching " + dquote(COLL) + \
		" must have length in the set {" + delimit(", ", deparseInts(lengths)) + "}.\n" + \
		"The actual length was " + deparseInt(coll_size) + ".";

		Function error_callback("error_callback");
		error_callback(message);
	}

}
