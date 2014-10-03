#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;












template<int T>
Vector<T> vector_map_template (const std::string FN, const List coll, const Function fn, const std::string type) {

	const int coll_size = coll.size();
	Vector<T> out(coll_size);



	std::map<std::string, int> int_type;

	int_type["integer"]   = INTSXP;
	int_type["double"]    = REALSXP;
	int_type["logical"]   = LGLSXP;
	int_type["complex"]   = CPLXSXP;
	int_type["raw"]       = RAWSXP;
	int_type["character"] = STRSXP;





	for (int ith = 0; ith < coll_size; ++ith) {

		Shield<SEXP> elem( fn(coll[ith]) );

		int elem_type = TYPEOF(elem);

		if (int_type[type] != elem_type) {

			std::stringstream msg;
			msg << "all values returned by ";
			msg << dquote(FN);
			msg << " must be of type ";
			msg << type;
			msg << ".\n";

			Function error_callback("error_callback");
			error_callback(msg.str(), 1);

		}

		Vector<T> wrapped = Rcpp::as< Vector<T> >(elem);

		if (wrapped.size() != 1) {

			std::stringstream msg;
			msg << "all values returned by ";
			msg << dquote(FN);
			msg << " must be length-one.\n";

			Function error_callback("error_callback");
			error_callback(msg.str(), 1);

		}

	}

	return out;

}





// [[Rcpp::export]]
SEXP vector_map (const std::string FN, SEXP coll, const Function fn, const std::string type) {

	if (type == "integer") {

		return vector_map_template<INTSXP>(FN, coll, fn, type);

	} else if (type == "double") {

		return vector_map_template<REALSXP>(FN, coll, fn, type);

	} else if (type == "logical") {

		return vector_map_template<LGLSXP>(FN, coll, fn, type);

	} else if (type == "complex") {

		return vector_map_template<CPLXSXP>(FN, coll, fn, type);

	} else if (type == "raw") {

		return vector_map_template<RAWSXP>(FN, coll, fn, type);

	} else if (type == "character") {

		return vector_map_template<STRSXP>(FN, coll, fn, type);

	} else {

		stop("internal error: unimplemented vector type in 'as_typed_vector'");

	}

}
