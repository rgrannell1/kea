#include <Rcpp.h>
using namespace Rcpp;




/*
// [[Rcpp::export]]
List cFlatMap (const Function fn, const List& coll) {

	std::vector<SEXP> out;
	std::vector<std::string> out_names;

	const int coll_size = coll.size();

	if (coll_size == 0) {
		return List::create();
	} else {

		for (int ith = 0; ith < coll_size; ++ith) {

			List::iterator it = fn(coll[ith]);





			if (coll.attr("names") != R_NilValue) {

				std::vector<std::string> elem_names = coll.attr("names");
				out_names.insert(out_names.end(), elem_names.begin(), elem_names.end());

			}

			for (int jth = 0; jth < elem.size(); ++jth) {
				out.push_back(elem[jth]);
			}





		}

		return out;
	}
}
*/
