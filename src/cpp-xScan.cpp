#include <Rcpp.h>
#import <math.h>
using namespace Rcpp;



// tooo slow at the moment.

// [[Rcpp::export]]
List cScan (const Function fn, const SEXP val, const List coll) {

	const int coll_size = coll.size();

	if (coll_size == 0) {
		return List::create();
	} else {

		List scanned(coll_size + 1);
		scanned[0] = val;

		for (int ith = 0; ith < coll_size; ++ith) {
			scanned[ith + 1] = fn(scanned[ith], coll[ith]);
		}

		return scanned;
	}
}
