#include <Rcpp.h>
#include <stack>
using namespace Rcpp;





// [[Rcpp::export]]
List cFlatten (const NumericVector& num, const List coll) {

	const R_len_t coll_size = coll.size();
	const R_len_t num_size  = num.size();

	if (coll_size == 0 || num_size == 0) {
		return List::create();
	} else {

		// unlist the contents.
		// standard iterative tree-traversal.

		std::vector<SEXP> elements;

		std::stack<SEXP> nodes;
		std::stack<R_len_t> depths;

		nodes .push(coll);
		depths.push(1);

		while (nodes.size() > 0) {
			// recursive-nodes still unexplored.

			Shield<SEXP> node( nodes.top() );
			R_len_t depth = depths.top();

			nodes .pop();
			depths.pop();

			R_len_t node_type = TYPEOF(node);

			if (node_type == VECSXP || node_type == LISTSXP) {
				// the node is recursive.

				List elem = as<List>(node);

				for (R_len_t ith = 0; ith < elem.size(); ++ith) {

					nodes .push(elem[ith]);
					depths.push(depth + 1);

				}

			} else {
				// the node is non-recursive, so push it to the element list.

				elements.push_back(node);
			}

		}

		std::reverse(elements.begin(), elements.end());
		return wrap(elements);

	}
}
