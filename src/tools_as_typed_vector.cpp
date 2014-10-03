#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;












template<int TYPE>
Vector<TYPE> vector_map_template (const List coll, const Function fn, const std::string type) {

	const int coll_size = coll.size();
	Vector<TYPE> out(coll_size);




	std::map<std::string, int> int_type;

	int_type["integer"]   = INTSXP;
	int_type["double"]    = REALSXP;
	int_type["logical"]   = LGLSXP;
	int_type["complex"]   = CPLXSXP;
	int_type["raw"]       = RAWSXP;
	int_type["character"] = STRSXP;





	for (int ith = 0; ith < coll_size; ++ith) {

		SEXP elem      = fn(coll[ith]);
		int  elem_type = TYPEOF(elem);

		if (int_type[type] != elem_type) {

			std::stringstream msg;
			msg << "the collection ";
			msg << dquote("coll");
			msg << " must be a collection of type ";
			msg << type;
			msg << ".\n";

			Function error_callback("error_callback");
			error_callback(msg.str(), 1);

		}
	}

	return out;

}





// [[Rcpp::export]]
SEXP vector_map (SEXP coll, const Function fn, const std::string type) {

	if (type == "integer") {

		return vector_map_template<INTSXP>(coll, fn, type);

	} else if (type == "double") {

		return vector_map_template<REALSXP>(coll, fn, type);

	} else if (type == "logical") {

		return vector_map_template<LGLSXP>(coll, fn, type);

	} else if (type == "complex") {

		return vector_map_template<CPLXSXP>(coll, fn, type);

	} else if (type == "raw") {

		return vector_map_template<RAWSXP>(coll, fn, type);

	} else if (type == "character") {

		return vector_map_template<STRSXP>(coll, fn, type);

	} else {

		stop("internal error: unimplemented vector type in as_typed_vector");

	}

}
