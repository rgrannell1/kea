#include <Rcpp.h>
#import <math.h>
using namespace Rcpp;



// tooo slow at the moment.

// [[Rcpp::export]]
List cScan (const Function& fn, const SEXP val, const List& coll) {

	const R_len_t coll_size = coll.size();

	if (coll_size == 0) {
		return List::create();
	} else {

		List scanned(coll_size + 1);
		scanned[0] = val;

		for (R_len_t ith = 0; ith < coll_size; ++ith) {
			scanned[ith + 1] = fn(scanned[ith], coll[ith]);
		}

		return scanned;
	}
}
