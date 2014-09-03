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
	/* needed to avoid a type-error when unnamed. */
	CharacterVector coll_names = coll.attr("names") != R_NilValue ?
		coll.attr("names"):
		CharacterVector::create();

	if (coll_len == 0 || num_len == 0) {
		return List::create();
	} else {

		int ith       = 0;
		int current   = 0;

		int average_elems = (coll_len / num_mag) + (coll_len % num_mag != 0);





		return out;
	}

}
