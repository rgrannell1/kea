#include <Rcpp.h>
#include <stack>
using namespace Rcpp;










// [[Rcpp::export]]
List cFlatten (const NumericVector& num, const List& coll) {

	const int coll_size = coll.size();
	const int num_size  = num.size();

	if (coll_size == 0 || num_size == 0) {
		return List::create();
	} else {


		// standard iterative tree-traversal.

		std::vector<SEXP> out;
		std::stack<SEXP> nodes;

		nodes.push(coll);

		while (nodes.size() > 0) {

			Shield<SEXP> node( nodes.top() );
			nodes.pop();

			if (TYPEOF(node) == VECSXP || TYPEOF(node) == LISTSXP) {
				// the node is recursive.

				List elem = as<List>(node);

				for (int ith = 0; ith < elem.size(); ++ith) {
					nodes.push(elem[ith]);
				}

			} else {
				out.push_back(node);
			}

		}

		return wrap(out);

	}
}
