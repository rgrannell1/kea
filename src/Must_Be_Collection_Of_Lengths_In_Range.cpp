#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;



// nasty.
void Must_Be_Collection_Of_Lengths_In_Range (const std::string COLLS, const std::string LOWER, const std::string UPPER, const List& colls, const R_len_t lower, const R_len_t upper) {

	bool all_correct_length = true;
	const R_len_t colls_size    = colls.size();

	for (R_len_t ith = 0; ith < colls_size; ++ith) {

		List coll     = colls[ith];
		R_len_t coll_size = coll.size();

		if (lower > coll_size || coll_size > upper) {

			all_correct_length = false;
			break;

		}
	}

	if (!all_correct_length) {

		List to_delimit = List::create(deparseInt(lower), "...", deparseInt(upper));

		std::stringstream msg;
		msg << "The argument matching "
			<< dquote(COLLS)
			<< " must be a collection of collections with lengths in the set {"
			<< delimit(", ", to_delimit)
			<< "}.\n";

		Function error_callback("error_callback");
		error_callback(msg.str(), 1);

	}

}
