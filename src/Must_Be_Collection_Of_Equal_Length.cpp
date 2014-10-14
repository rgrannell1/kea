#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;





void Must_Be_Collection_Of_Equal_Length (const std::string COLLS, const List colls) {

	const int colls_size = colls.size();

	if (colls_size == 0) {
		return;
	}

	const List &first    = colls[0];
	const int elem_size  = first.size();

	bool all_equal = true;

	for (int ith = 0; ith < colls_size; ++ith) {
		const List &elem = colls[ith];

		all_equal = all_equal && (elem.size() == elem_size);
	}

	if (!all_equal) {

		std::stringstream msg;
		msg << "The argument matching "
			<< dquote(COLLS)
			<< " must be a collection of collections with equal lengths.";

		Function error_callback("error_callback");
		error_callback(msg.str(), 1);
	}
}
