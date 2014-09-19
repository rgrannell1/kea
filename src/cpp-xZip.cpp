#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;






// [[Rcpp::export]]
List cZip (const List colls) {

	const int colls_size = colls.size();

	if (colls_size == 0) {
		return List::create();
	} else {

		Must_Be_Collection_Of_Equal_Length("colls", colls);

		const List &first   = colls[0];
		const int elem_size = first.size();

		if (elem_size == 0) {
			return List::create();
		} else {

			List out(elem_size);

			const bool has_names = colls.attr("names") != R_NilValue;

			CharacterVector colls_names(colls_size);

			if (has_names) {
				colls_names = colls.attr("names");
			}

			for (int ith = 0; ith < elem_size; ++ith) {

				List row(colls_size);

				for (int jth = 0; jth < colls_size; ++jth) {

					List coll = colls[jth];
					row[jth] = coll[ith];
				}

				if (has_names) {
					row.attr("names") = colls_names;
				}

				out[ith] = row;
			}

			return out;
		}
	}
}
