#include <Rcpp.h>
using namespace Rcpp;





std::string delimit (const std::string str, const List strs) {

	const int strs_size = strs.size();

	if (strs_size == 0) {
		return "";
	} else if (strs_size == 1) {
		return strs[0];
	} else {

		return "asasdasd";

	}
}




std::string deparseInt (const int num) {

	std::string out;
	std::ostringstream os;

	os << num;

	return os.str();
}



/*
	dquote

	wrap a string in nice quotation marks.
*/

std::string dquote (const std::string str) {
	return "“" + str + "”";
}





void Must_Be_Of_Length (const std::string COLL, const List coll, const NumericVector lengths) {

	const int coll_size    = coll.size();
	const int lengths_size = lengths.size();

	bool match_found       = false;

	for (int ith = 0; ith < lengths_size; ++ith) {
		if (coll_size == lengths[ith]) {
			match_found = true;
		}
	}

	if (!match_found) {

		const std::string message = "The argument matching " + dquote(COLL) + \
		" must have length in the set {" + delimit(",", "1") + "}.\n" + \
		"The actual length was " + ".";

		stop(message);
	}

}





// [[Rcpp::export]]
List cZipKeys (const List colls) {

	const int colls_size = colls.size();

	if (colls_size == 0) {
		return List::create();
	} else {

		List out (colls_size);
		CharacterVector keys(colls_size);

		for (int ith = 0; ith < colls_size; ++ith) {

			List coll  = colls[ith];

			Must_Be_Of_Length("key", coll[0], 1);

			String key = coll[0];

			out[ith]   = coll[1];
			keys[ith]  = key;

 		}

		out.attr("names") = keys;

		return out;
	}

}
