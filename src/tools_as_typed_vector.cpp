#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;

// [[Rcpp::export]]
List testfun0 (const List& coll) {

	const int coll_size = coll.size();
	List out(coll_size);

	for (int ith = 0; ith < coll_size; ++ith) {
		out[ith] = ((List)coll[ith]).size();
	}

	return out;

}


// [[Rcpp::export]]
List testfun1 (const List& coll) {

	const int coll_size = coll.size();
	List out(coll_size);

	for (int ith = 0; ith < coll_size; ++ith) {
		List elem = coll[ith];
		out[ith] = elem.size();
	}

	return out;

}


// [[Rcpp::export]]
List testfun2 (const List& coll) {

	const int coll_size = coll.size();
	std::vector<int> out;

	for (int ith = 0; ith < coll_size; ++ith) {

		List elem = coll[ith];
		out.push_back(elem.size());

	}

	return wrap(out);
}


// [[Rcpp::export]]
List testfun3 (const List& coll) {

	const int coll_size = coll.size();
	std::vector<int> out;

	for (int ith = 0; ith < coll_size; ++ith) {
		out.push_back(((List)coll[ith]).size());
	}

	return wrap(out);
}
