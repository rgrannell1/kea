#include <Rcpp.h>
#include <stdlib.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cCycle (NumericVector num, List coll) {

	int num_len  = num.size();
	int coll_len = coll.size();

	int num_mag  = num[0];

	bool has_names             = coll.attr("names") != R_NilValue;
	CharacterVector coll_names = coll.attr("names") != R_NilValue ? coll.attr("names"): CharacterVector::create();

	if (coll_len == 0 || num_len == 0) {

		return List::create();

	} else {

		List out(coll_len);
		List out_names(coll_len);

		for (int ith = 0; ith < coll_len; ith++) {

			int index = abs((num_mag + ith)) % coll_len;
			out[ith]  = coll[index];

			if (has_names) {
				out_names[ith] = coll_names[index];
			}

		}

		if (has_names) {
			out.attr("names") = out_names;
		}

		return out;
	}
}
