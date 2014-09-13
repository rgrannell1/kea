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
		std::string delimiter = Rcpp::as<std::string>(str[0]);

		for (int ith = 0; ith < strs_size; ++ith) {

			os << strs[ith];

			if (ith != strs_size - 1) {
				os << delimiter;
			}
		}

		return os.str();
	}
}
