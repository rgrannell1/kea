#include <Rcpp.h>
using namespace Rcpp;





// [[Rcpp::export]]
List cRecycle (const NumericVector& num, const List& coll) {

	const R_len_t num_len    = num.size();
	const R_len_t coll_len   = coll.size();

	const bool has_names = coll.attr("names") != R_NilValue;

	if (coll_len == 0 || num_len == 0 || num[0] == 0) {

		List out(0);

		if (has_names) {
			out.attr("names") = CharacterVector::create();
		}

		return out;

	} else {

		const R_len_t num_mag = num[0];


		if (has_names) {

			List out(num_mag);
			CharacterVector out_names(num_mag);

			CharacterVector coll_names = coll.attr("names");

			for (R_len_t ith = 0; ith < num_mag; ++ith) {

				R_len_t index = ith % coll_len;

				out[ith]       = coll[index];
				out_names[ith] = coll_names[index];
			}

			out.attr("names") = out_names;

			return out;

		} else {

			List out(num_mag);

			for (R_len_t ith = 0; ith < num_mag; ++ith) {
				out[ith] = coll[ith % coll_len];
			}

			return out;

		}

	}
}
