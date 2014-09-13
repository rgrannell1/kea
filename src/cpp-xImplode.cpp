#include <Rcpp.h>
#import <math.h>
#include "functions.h"
using namespace Rcpp;




// [[Rcpp::export]]
CharacterVector cImplode (const CharacterVector str, const CharacterVector strs) {

	const int str_size  = str.size();
	const int strs_size = strs.size();

	if (str_size == 0 || strs_size == 0) {
		return CharacterVector::create();
	} else {

		std::ostringstream os;

		for (int ith = 0; ith < strs_size; ++ith) {

			os << strs[ith];
			os << str;
		}

		return os.str();
	}
}
