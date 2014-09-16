#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;

// Worst case should be O(n^2), when there are n distinct elements.





// [[Rcpp::export]]
List cGroupBy (Function fn, List coll) {
	return List::create();
}
