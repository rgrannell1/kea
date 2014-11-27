#include <Rcpp.h>
using namespace Rcpp;










// [[Rcpp::export]]
List cProdSetOf (const List& colls) {

	R_len_t out_size   = 1;
	const R_len_t colls_size = colls.size();

	std::vector<R_len_t> coll_sizes;
	std::vector<R_len_t> tuple_index;

	// get the size of each collection, populate initial
	// indices, and product of sizes.

	for (R_len_t ith = 0; ith < colls_size; ++ith) {

		List elem = colls[ith];

		coll_sizes .push_back(elem.size());
		tuple_index.push_back(0);

		out_size *= elem.size();

	}

	if (out_size == 0) {
		return List::create();
	}

	R_len_t out_ith = 0;
	List out(out_size);

	while (out_size > out_ith) {

		List out_tuple(colls_size);

		// add the current ordered tuple to the output.
		for (R_len_t ith = 0; ith < colls_size; ++ith) {

			List coll       = colls[ith];
			out_tuple[ith]  = coll[tuple_index[ith]];

		}

		out[out_ith] = out_tuple;
		++out_ith;

		R_len_t ith = colls_size - 1;

		// get the next product-set indices.
		while (true) {

			++tuple_index[ith];

			if (tuple_index[ith] > coll_sizes[ith] - 1 && ith > 0) {

				tuple_index[ith] = 0;
				--ith;

			} else {
				break;
			}

		}

	}

	return out;

}
