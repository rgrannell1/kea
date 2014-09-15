#include <Rcpp.h>
using namespace Rcpp;



//TOO SLOW at the moment.

// [[Rcpp::export]]
List cJoin (const List colls) {

	const int colls_size = colls.size();

	if (colls_size == 0) {
		return List::create();
	} else {

		int elements = 0;

		for (int ith = 0; ith < colls_size; ++ith) {

			List coll = colls[ith];
			elements  += coll.size();

		}

		int elem_ith = 0;

		List out(elements);
		CharacterVector out_names(elements);

		for (int ith = 0; ith < colls_size; ++ith) {

			List coll      = colls[ith];
			int coll_size  = coll.size();

			bool has_names = coll.attr("names") != R_NilValue;

			CharacterVector coll_names(coll_size);

			if (has_names) {
				coll_names = coll.attr("names");
			}

			for (int jth = 0; jth < coll_size; ++jth) {

				out[elem_ith]       = coll[jth];
				out_names[elem_ith] = has_names ? coll_names[jth]: "";

				++elem_ith;
			}
		}

		out.attr("names") = out_names;
		return out;
	}

}
