
# Must_Have_Canonical_Arguments
#
# R has a problem with named variadic arguments:
#
# check that the names aren't interpreted differently
# between sys.call (your interpretation of the arguments) and
# match.call (the system's interpretation). This fixed an
# odd issue in R"s function call semantics
#
# xFix_(function (fn, b) fn(b), fn = xI)
#
# is misinterpreded by R; fn is not used as an ellipsis arg, but an arg to xFix.
# libraries like plyr use .fn to try get around this; Kiwi uses this odd middleware macro.
# It'll throw an error for argument lists that are ambigious.
#

Must_Have_Canonical_Arguments <- function () {

	bquote({

		.sys_call   <- sys.call()
		.match_call <- match.call()

		# -- check if any of the user-supplied names are moved in the matched
		# -- call from the supplied call.

		if (!all( names(.sys_call) == names(.match_call) | names(.sys_call) == '')) {

			fn_name <- .sys_call[[1]]

			if (length(fn_name) == 1 && is.name(fn_name)) {

				fn_name   <- paste(fn_name)
				suggested <- gsub('_', '', fn_name)

			} else {

				fn_name   <- 'xMethod_'
				suggested <- 'xMethod'

			}

			invoked <- names(.sys_call)
			matched <- names(.match_call)

			ambigious <- invoked[which(invoked != matched & invoked != '')]

			message <-
				"The ellipsis argument explicitly named " %+% dQuote(ambigious) %+% " matches a parametre " %+%
				"of " %+% dQuote(fn_name)  %+% ". This will be misinterpreted by R. Use " %+%
				dQuote(suggested) %+% " with a list argument instead of " %+% dQuote(fn_name) %+% "."

			throw_kiwi_error(.sys_call, message)
		}

	})
}
