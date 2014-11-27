#include <Rcpp.h>
#import <math.h>
#include "functions.h"
using namespace Rcpp;




// [[Rcpp::export]]
CharacterVector cImplode (const CharacterVector str, const CharacterVector& strs) {

	const R_len_t str_size  = str.size();
	const R_len_t strs_size = strs.size();

	if (str_size == 0 || strs_size == 0) {
		return CharacterVector::create();
	} else {

		std::ostringstream os;
		std::string delimiter = Rcpp::as<std::string>(str[0]);

		for (R_len_t ith = 0; ith < strs_size; ++ith) {
			ith != strs_size - 1 ? os << strs[ith] << delimiter: os << strs[ith];
		}

		return os.str();
	}
}
