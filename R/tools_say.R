
# -------------------------------- proclaim -------------------------------- #
#
# To Developers,
#
# lament is virtually identical to exclaim in its implementation and purpose,
# except that it is used for the error messages that xLambda and only xLambda
# generates; there is no point bloating exclaim.


proclaim <- list(
	incorrent_delimiter =
		function (actual, expected) {

			"the unexpected delimiter " %+% dQuote(actual) %+%
			" was encountered, but the delimiter " %+% dQuote(expected) %+%
			" was expected."

		},
	non_symbol_param =
		function (actual) {

			"function parametres must be symbols." %+%
			summate(actual)

		},
	no_enclosing_parens =
		function () {

			"the formals for non-unary functions" %+%
			" must be enclosed in parentheses."

		}
)

# -------------------------------- lament -------------------------------- #
#
# To Developers,
#
# lament is virtually identical to exclaim in its implementation and purpose,
# except that it is not used by the core arrow library; it is used by the
# forall( ) function, for throwing its errors.
#
# It is named lament, as lament is called only when a unit test is failed, in
# which case I am less than happy.
#


lament <- list(
	null_cases =
		function (info, profile = '') {

			param <- paste(match.call()$param)

			write_error(info, "\n",
				dQuote("cases"), " must not be null.",
				call. = False)
		},
	non_function_cases =
		function (info, profile = '') {

			param <- paste(match.call()$param)

			write_error(info, "\n",
				dQuote("cases"), " must be a list of functions.",
				call. = False)
		},
	non_boolean_expectation =
		function (info, case, profile = '') {

			write_error(info, "\n",
				"expectation returned a non-boolean ",
				"value when called with \n\n",
				ddparse(case), call. = False)
		},
	non_singular_expectation =
		function (info, len, profile = '') {

			write_error(info, "\n",
				"expectation returned a non-length-one ",
				"value (actual length was ", len , ")", call. = False)
		},
	failed_cases =
		function (info, after, failed, profile = '') {

			cases <- sapply(lapply(failed, unname), ddparse)
			cases <- newline(cases[ seq_along( min(10, length(cases)) ) ])

			write_error(info, "\n",
				"failed after the ", ith_suffix(after),
				" case!\n\n", cases, "\n",
				call. = False)
		}
)

# -------------------------------- wail -------------------------------- #
#
# To Developers,
#
# wail is specific to customised errors thrown by the arrow method chaining unit tests,
# particularily the more complicated ones.
#
# It is named wail after the noise I emit when a unit test fails...

wail <- list(
	normal_form_missing =
		function (method, proto, actual) {

			'the xMethod form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'
		},
	unchaining_form_missing =
		function (method, proto, actual) {

			'the x_Method form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'

		},
	variadic_form_missing =
		function (method, proto, actual) {
			'the xMethod... form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'

		},
	variadic_unchaining_form_missing =
		function (method, proto, actual) {
			'the x_Method... form of ' %+% method %+% ' was expected but ' %+%
			' was missing (' %+% proto %+% ')'

		},
	method_not_in_proto =
		function (method, proto) {
			'the method ' %+% method %+% ' should be in the prototype ' %+% proto %+%
			' but was not.'

		},
	unchaining_calls_x_ =
		function (method) {
			'the method ' %+% method %+%
			' was supposed to be unchaining but called x_'

		},
	chaining_must_call_x_ =
		function (method) {
			'the method ' %+% method %+%
			' was supposed to be chaining but didnt call x_'

		},
	variadic_must_call_... =
		function (method) {

			'the method ' %+% method %+%
			' was supposed to be variadic but didnt call ...'

		},
	non_variadic_calls_... =
		function (method) {

			'the method ' %+% method %+%
			' was supposed to be non-variadic but called ...'

		},
	function_not_in_method =
		function (method) {

			'the method ' %+% method %+%
			' should have called its underlying function.'
		}
)

# -------------------------------- yelp -------------------------------- #
#
# To Developers,
#
# yelp stores the error messages specific to assert itself.

yelp <- list(
	assertion_failed =
		function (calltext, expr) {

			expr <- ddparse(expr)

			calltext %+% ': the assertion\n ' %+% expr %+% ' \nfailed.'

		},
	non_logical_assertion =
		function (expr) {

			expr <- ddparse(expr)

			"the assertion " %+% expr %+% " returned a non-logical value."
		},
	arrow_function_failed =
		function (callname, callinfo, message) {

			stopifnot(is.character(message))

			callname <- paste0(callname, collapse = '')
			callinfo <- wrap(callinfo, indent = 4)


			overview <-
			'\n[ error thrown from ' %+% callname %+% ' ]:\n\n'

			overview %+%
			callinfo %+% '\n\n' %+%
			'[ details ]:\n\n' %+%
			message

		},
	warning_higher_order =
		function (fn, warn, profile = '') {

			warnmessage <-
				paste0(warn$message, collapse = '')

			inner_call <- stringify_call(warn$call) %+% ":\n\n"

			overview <-
			"[ warning occurred while executing a function passed to " %+% fn %+% " ]\n\n"

			inner_call <- paste0('    ', inner_call)

			warnmessage <- strsplit(warnmessage, '\n')[[1]]
			warnmessage <- paste0('    ', warnmessage, collapse = '\n')

			overview %+% inner_call %+% warnmessage
		},
	error_higher_order =
		function (fn, err, profile = '') {

			errmessage <-
				paste0(err$message, collapse = '')

			inner_call <- stringify_call(err$call) %+% ':\n\n'

			overview <-
			"[ an error occurred while executing a function passed to " %+% fn %+% " ]:\n\n"

			inner_call <- paste0('    ', inner_call)

			errmessage <- strsplit(errmessage, '\n')[[1]]
			errmessage <- paste0('    ', errmessage, collapse = '\n')

			overview %+% inner_call %+% errmessage
		}
)
