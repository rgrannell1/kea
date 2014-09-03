#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cChunk (NumericVector num, List coll) {

	int num_len  = num.size();
	int coll_len = coll.size();

	int num_mag;

	if (num[0] == INFINITY) {
		num_mag = coll_len + 1;
	} else {
		num_mag = num[0];
	}

	bool has_names             = coll.attr("names") != R_NilValue;
	CharacterVector coll_names = coll.attr("names") != R_NilValue ? coll.attr("names"): CharacterVector::create();

	if (coll_len == 0 || num_len == 0) {
		return List::create();
	} else {

		int lower   = 0;
		int out_len = (coll_len / num_mag) + (coll_len % num_mag != 0);

		List out(out_len);

		for (int ith = 0; ith < out_len; ith++) {

			int upper;

			if (lower + num_mag < coll_len + 1) {
				upper = num_mag;
			} else {
				upper = coll_len - lower;
			}

			List chunk(upper);
			CharacterVector chunk_names(upper);

			for (int jth = 0; jth < upper; jth++) {

				int index  = lower + jth;
				chunk[jth] = coll[index];

				if (has_names) {
					chunk_names[jth] = coll_names[index];
				}
			}

			chunk.attr("names") = chunk_names;
			out[ith] = chunk;

			lower += num_mag;

		}

		return out;
	}

}
