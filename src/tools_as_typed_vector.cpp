#include <Rcpp.h>
using namespace Rcpp;




template<int TYPE>
Vector<TYPE> vector_map_template (const List coll, const Function fn, const std::string type) {

	const int coll_size = coll.size();
	Vector<TYPE> out(coll_size);

	for (int ith = 0; ith < coll_size; ++ith) {

		SEXP elem = fn(coll[ith]);

	}

	return out;

}

// [[Rcpp::export]]
SEXP vector_map (SEXP coll, const Function fn, const std::string type) {

	const int coll_type = TYPEOF(coll);

	if (coll_type != VECSXP) {
		// atomic vectors.

		bool all_is_na = true;

		if (coll_type == INTSXP) {

			IntegerVector vect = as<IntegerVector>(coll);

			/*
			for (int ith = 0; ith < vect.size(); ++ith) {

				IntegerVector elem = vect[ith];

				if ( is_na(elem) ) {
					all_is_na = false;
					break;
				}

			}
			*/

		} else if (coll_type == REALSXP) {

			DoubleVector vect = as<DoubleVector>(coll);

		} else if (coll_type == LGLSXP) {

			LogicalVector vect = as<LogicalVector>(coll);

		} else if (coll_type == CPLXSXP) {

			ComplexVector vect = as<ComplexVector>(coll);

		} else if (coll_type == RAWSXP) {

			RawVector vect = as<RawVector>(coll);

		} else if (coll_type == STRSXP) {

			CharacterVector vect = as<CharacterVector>(coll);

		}


			/*
		for (int ith = 0; ith < coll.size(); ++ith) {

			if ( !is_true(is_na(coll[ith];)) ) {
				all_is_na = false;
				break;
			}

		}



		if (all_is_na) {
			//


		} else {
			return coll;
		}*/

	} else {
		// generic vectors.


	}





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
