#include <Rcpp.h>
#include <stack>
using namespace Rcpp;





// [[Rcpp::export]]
List cFlatten (const NumericVector& num, const List coll) {

	const int coll_size = coll.size();
	const int num_size  = num.size();

	if (coll_size == 0 || num_size == 0) {
		return List::create();
	} else {

		// unlist the contents.
		// standard iterative tree-traversal.

		std::vector<SEXP> out;

		std::stack<SEXP> nodes;
		std::stack<int> depths;

		nodes .push(coll);
		depths.push(1);

		while (nodes.size() > 0) {
			// recursive-nodes still unexplored.

			Shield<SEXP> node( nodes.top() );
			int depth = depths.top();

			nodes .pop();
			depths.pop();

			if (TYPEOF(node) == VECSXP || TYPEOF(node) == LISTSXP) {
				// the node is recursive.

				List elem = as<List>(node);

				for (int ith = 0; ith < elem.size(); ++ith) {

					nodes .push(elem[ith]);
					depths.push(depth + 1);

				}

			} else {
				// the node is non-recursive.
				out.push_back(node);
			}

		}

		std::reverse(out.begin(), out.end());
		return wrap(out);

	}
}