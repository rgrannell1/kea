#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;






// [[Rcpp::export]]
List cZip (const List& colls) {

	const R_len_t colls_size = colls.size();

	if (colls_size == 0) {
		return List::create();
	} else {

		Must_Be_Collection_Of_Equal_Length("colls", colls);

		const R_len_t elem_size = ((List)colls[0]).size();

		if (elem_size == 0) {
			return List::create();
		} else {

			List out(elem_size);

			const bool has_names = colls.attr("names") != R_NilValue;

			CharacterVector colls_names(colls_size);

			if (has_names) {
				colls_names = colls.attr("names");
			}

			List row_template(colls_size);

			if (has_names) {
				row_template.attr("names") = colls_names;
			}

			for (R_len_t ith = 0; ith < elem_size; ++ith) {

				List row = clone(row_template);

				for (R_len_t jth = 0; jth < colls_size; ++jth) {
					row[jth]  = ((List)colls[jth])[ith];
				}

				out[ith] = row;
			}

			return out;
		}
	}
}
