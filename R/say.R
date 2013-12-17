
# -------------------------------- exclaim -------------------------------- #
#
# To Developers,
# exclaim is a list of functions that return strings that form arrow's errors. They are factored
# into functions for reusability; repeating work is the easiest way to make a mistake.
#
# The functions themselves are faily self explainatory; they are usually
# involved by assert( ), a function in utilities used to test expressions.
# Assert will - if not given a better message - generate its own error,
# but it will be less than informative.
#
# A typical use of exclaim:
#
# assert(
#    !missing(pred), invoking_call,
#    exclaim$parameter_missing(pred))
#
# invoking_call gives the exact command used to call the function that
# threw the error.
#

exclaim <- list(
	parameter_missing =
		function (param) {
			param <- paste(match.call()$param)

			"the parameter " %+% dQuote(param) %+% " is required but was missing."
		},
	must_be_matchable =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a function, or a symbol or string" %+%
			" that can be looked-up as a function."

		},
	must_be_nameable =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" a symbol or string."
		},
	must_be_collection =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a list, a pairlist or a typed vector."
		},
	must_be_function =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a function."

		},
	must_be_collection_of_length =
		function (param, length) {

			param <- paste(match.call()$param)

			"the function matching " %+% dQuote(param) %+%
			" must be a collection of length " %+% length %+% " values."

		},
	must_be_unary =
		function (param) {
			param <- paste(match.call()$param)

			"the function matching " %+% dQuote(param) %+%
			" must be a unary function."
		},
	must_be_binary =
		function (param) {
			param <- paste(match.call()$param)

			"the function matching " %+% dQuote(param) %+%
			" must be a binary function."
		},
	must_be_recursive =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a list or a pairlist."
		},
	must_be_recursive_of_matchable =
		function (param) {

			param <- paste(match.call()$param)

			"the arguments matching " %+% dQuote(param) %+%
			" must all be functions, or symbols or strings" %+%
			" that can be looked-up as functions."

		},
	must_be_numeric =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a double or an integer."

		},
	must_be_character =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a character vector."

		},
	must_be_string	 =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a length one character vector."

		},
	must_be_whole =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a whole number."
		},
	must_have_length =
		function (param, lengths) {

			param <- paste(match.call()$param)

			lengths <- paste(lengths, collapse = " or ")

			"the argument matching " %+% dQuote(param) %+%
			" must have length " %+% lengths

		},
	must_be_longer_than =
		function (param, length) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must have length longer than " %+% length

		},
	must_be_lequal_than =
			function (param, length) {

				param <- paste(match.call()$param)

				"the argument matching " %+% dQuote(param) %+%
				" must have length equal or longer than " %+% length

			},
	must_be_greater_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " %+% dQuote(param) %+%
			" must be larger than " %+% size

		},
	must_be_greater_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " %+% dQuote(param) %+%
			" must be greater than " %+% size

		},
	must_be_grequal_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " %+% dQuote(param) %+%
			" must be greater or equal to " %+% size

		},
	must_be_recursive_of_collections =
		function (param) {

			param <- paste(match.call()$param)

			"the arguments matching " %+% dQuote(param) %+%
			" must all be lists, vectors or pairlists."

		},
	type_coersion_failed =
		function (param, mode) {

			# does not match param; param is handed in as a symbol.

			"the arguments matching " %+% dQuote(param) %+%
			" must be a list or pairlist of " %+% mode %+% "s" %+%
			", or a " %+% mode %+% " vector."

		},
	method_not_found =
		function (name, contents_are, similar) {

			if (length(similar) == 0) {
				"could not find the method " %+% name %+% "."
			} else {
				"could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+%
				":\n" %+%
				"did you mean " %+% sample(similar, size = 1) %+% "?"
			}
		},
	must_be_named =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" must be a named collection."
		},
	must_have_equal_lengths =
		function (name1, name2) {

			name1 <- paste(match.call()$name1)
			name2 <- paste(match.call()$name2)

			"both " %+% name1 %+% " and " %+% name2 %+%
			" must have equal lengths."
		},
	variable_non_existent =
		function (name) {

			name <- paste(match.call()$param)

			"no variable exists by the name " %+% name
		},
	binding_is_locked =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% dQuote(param) %+%
			" cannot point to a locked variable."
		},
	must_be_params_of =
		function (names, fn) {

			names <- paste(match.call()$names)
			fn <- paste(match.call()$fn)

			"the elements of " %+% names %+%
			" must be parametre names of " %+% fn
		},
	warning_higher_order =
		function (fn, warn) {

			"a warning occurred while executing a function passed to " %+% fn %+% ":\n" %+%
			dQuote(warn$message)

		},
	error_higher_order =
		function (fn, err) {

			"an error occurred while executing a function passed to " %+% fn %+% ":\n\n" %+%
			dQuote(err$message)

		}
)



# -------------------------------- lament -------------------------------- #
#
# To Developers,
# lament is virtually identical to exclaim in its implementation and purpose,
# except that it is not used by the core arrow library; it is used by the
# forall( ) function, for throwing its errors.
#
# It is named lament, as lament is called only when a unit test is failed, in
# which case I am less than happy.
#

ddparse <- function (val, collapse = "") {
	paste0(deparse(val), collapse = collapse)
}
newline <- function (val) {
	paste0(val, collapse = "\n")
}

lament <- list(
	null_cases =
		function (info) {

			param <- paste(match.call()$param)

			stop(info, "\n",
				dQuote("cases"), " must not be null.",
				call. = False)
		},
	non_function_cases =
		function (info) {

			param <- paste(match.call()$param)

			stop(info, "\n",
				dQuote("cases"), " must be a list of functions.",
				call. = False)
		},
	non_boolean_expectation =
		function (info, case) {

			stop(info, "\n",
				"expectation returned a non-boolean ",
				"value when called with \n\n",
				ddparse(case), call. = False)
		},
	non_singular_expectation =
		function (info, len) {

			stop(info, "\n",
				"expectation returned a non-length-one ",
				"value (actual length was ", len , ")", call. = False)
		},
	failed_cases =
		function (info, after, failed) {

			cases <- sapply(lapply(failed, unname), ddparse)
			cases <- newline(cases[ seq_along( min(10, length(cases)) ) ])

			stop(info, "\n",
				"failed after the ", ith_suffix(after),
				" case!\n\n", cases, "\n",
				call. = False)
		}
)

