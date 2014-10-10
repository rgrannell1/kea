#include <Rcpp.h>
#include "functions.h"
using namespace Rcpp;





void Must_Be_Flag (const std::string& PRED, const SEXP& flag) {

	if (TYPEOF(flag) != LGLSXP) {

		const std::string message =
			"The predicate function " + dquote(PRED) + \
			" produced a non-{True, False, Na} value.\n";

		Function error_callback("error_callback");
		error_callback(message, 1);

	} else {

		const LogicalVector logical_flag = as<LogicalVector>(flag);

		if (logical_flag.size() != 1) {

			std::stringstream msg;
			msg << "The predicate function "
				<< dquote(PRED)
				<< " produced a non-{True, False, Na} value.\n";

			Function error_callback("error_callback");
			error_callback(msg.str(), 1);

		}

	}
}
