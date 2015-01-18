#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;





void Must_Be_Of_Length (const std::string COLL, const List coll, const List lengths) {

	const R_len_t coll_size    = coll.size();
	const R_len_t lengths_size = lengths.size();

	bool match_found       = false;

	for (R_len_t ith = 0; ith < lengths_size; ++ith) {
		if (coll_size == lengths[ith]) {
			match_found = true;
		}

		if (!match_found) {
			break;
		}
	}

	if (!match_found) {

		const std::string message = "The argument matching " + dquote(COLL) + \
		" must have length in the set {" + delimit(", ", deparseInts(lengths)) + "}.\n" + \
		"The actual length was " + deparseInt(coll_size) + ".\n";

		Function error_callback("error_callback");
		error_callback(message, "value_error", 1);
	}

}
