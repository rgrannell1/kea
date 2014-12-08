#include <Rcpp.h>
using namespace Rcpp;






// [[Rcpp::export]]
List cChunk (NumericVector num, const List& coll) {

	const R_len_t num_len  = num.size();
	const R_len_t coll_len = coll.size();

	R_len_t num_mag;

	if (num[0] == INFINITY) {
		num_mag = coll_len + 1;
	} else {
		num_mag = num[0];
	}

	const bool has_names             = coll.attr("names") != R_NilValue;
	/* needed to avoid a type-error when unnamed. */
	CharacterVector coll_names = coll.attr("names") != R_NilValue ?
		coll.attr("names"):
		CharacterVector::create();

	if (coll_len == 0 || num_len == 0) {
		return List::create();
	} else {

		R_len_t lower   = 0;
		R_len_t out_len = (coll_len / num_mag) + (coll_len % num_mag != 0);

		List out(out_len);

		for (R_len_t ith = 0; ith < out_len; ++ith) {

			R_len_t upper;

			if (lower + num_mag < coll_len + 1) {
				upper = num_mag;
			} else {
				upper = coll_len - lower;
			}

			List chunk(upper);
			CharacterVector chunk_names(upper);

			for (R_len_t jth = 0; jth < upper; ++jth) {

				R_len_t index  = lower + jth;
				chunk[jth] = coll[index];

				if (has_names) {
					chunk_names[jth] = coll_names[index];
				}
			}

			chunk.attr("names") = chunk_names;
			out[ith]            = chunk;

			lower += num_mag;

		}

		return out;
	}

}
