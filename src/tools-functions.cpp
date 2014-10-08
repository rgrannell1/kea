#include <Rcpp.h>
using namespace Rcpp;





/*
	typed_map
*/




/*
	delimit

	collapse a List of string s with a string delimiter.
*/

std::string delimit (const std::string str, const List strs) {

	const int strs_size = strs.size();

	if (strs_size == 0) {
		return "";
	} else if (strs_size == 1) {
		return strs[0];
	} else {

		std::ostringstream os;
		const int strs_size = strs.size();

		for (int ith = 0; ith < strs_size; ++ith) {

			std::string elem = strs[ith];
			ith != strs_size - 1 ? os << str + elem: os << elem;

		}

		return os.str();
	}
}


/*
	deparseInt

	convert an integer to a string representation.
*/

std::string deparseInt (const int num) {

	std::string out;
	std::ostringstream os;

	os << num;

	return os.str();
}



/*
	deparseInts

	deparse a list of integers to a list of strings.
*/

List deparseInts (const List nums) {

	const int nums_size = nums.size();
	List out(nums_size);

	for (int ith = 0; ith < nums_size; ++ith) {
		out[ith] = deparseInt(nums[ith]);
	}

	return out;
}


/*
	dquote

	wrap a string in nice quotation marks.
*/

std::string dquote (const std::string str) {
	return "“" + str + "”";
}



/*
	concat

	join two lists.
*/

List concat (List coll0, List coll1) {

	List out (coll0.size() + coll1.size());

	int ith = 0;

	for (int jth = 0; jth < coll0.size(); ++jth) {
		out[ith] = coll0[jth];
		++ith;
	}

	for (int jth = 0; jth < coll1.size(); ++jth) {
		out[ith] = coll1[jth];
		++ith;
	}

	return out;
}





std::vector<int> indices_to (int num) {

	std::vector<int> out;

	for (int ith = 0; ith < num; ++ith) {
		out.push_back(ith);
	}

	return out;
}
