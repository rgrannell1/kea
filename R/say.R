
# -------------------------------- exclaim -------------------------------- #
#
# error messages for the forall testing function.
#

exclaim <- list(
	parameter_missing =
		function (param) {
			param <- paste(match.call()$param)

			"the parameter " %+% param %+% " is required but was missing."
		},
	must_be_matchable =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a function, or a symbol or string" %+%
			" that can be looked-up as a function."
		},
	must_be_nameable =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" a symbol or string."
		},
	must_be_collection =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a list, a pairlist or a typed vector."

		},
	must_be_collection_of_length =
		function (param, length) {

			param <- paste(match.call()$param)

			"the function matching " %+% param %+%
			" must be a collection of length " %+% length %+% " values."

		},
	must_be_unary =
		function (param) {
			param <- paste(match.call()$param)

			"the function matching " %+% param %+%
			" must be a unary function."
		},
	must_be_binary =
		function (param) {
			param <- paste(match.call()$param)

			"the function matching " %+% param %+%
			" must be a binary function."
		},
	must_be_recursive =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a list or a pairlist."
		},
	must_be_recursive_of_matchable =
		function (param) {

			param <- paste(match.call()$param)

			"the arguments matching " %+% param %+%
			" must all be functions, or symbols or strings" %+%
			" that can be looked-up as functions."

		},
	must_be_numeric =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a double or an integer."

		},
	must_be_character =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a character vector."

		},
	must_be_string	 =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a length one character vector."

		},
	must_be_whole =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a whole number."
		},
	must_have_length =
		function (param, lengths) {

			param <- paste(match.call()$param)

			lengths <- paste(lengths, collapse = " or ")

			"the argument matching " %+% param %+%
			" must have length " %+% lengths

		},
	must_be_longer_than =
		function (param, length) {

			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must have length longer than " %+% length

		},
	must_be_lequal_than =
			function (param, length) {

				param <- paste(match.call()$param)

				"the argument matching " %+% param %+%
				" must have length equal or longer than " %+% length

			},
	must_be_greater_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " %+% param %+%
			" must be larger than " %+% size

		},
	must_be_greater_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " %+% param %+%
			" must be greater than " %+% size

		},
	must_be_grequal_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " %+% param %+%
			" must be greater or equal to " %+% size

		},
	must_be_recursive_of_collections =
		function (param) {

			param <- paste(match.call()$param)

			"the arguments matching " %+% param %+%
			" must all be lists, vectors or pairlists."

		},
	type_coersion_failed =
		function (param, mode) {

			param <- paste(match.call()$param)

			"the arguments matching " %+% param %+%
			" must be a list or pairlist of " %+% mode %+% "s" %+%
			", or a " %+% mode %+% " vector."

		},
	method_not_found =
		function (name, similar) {

			if (length(similar) == 0) {
				"could not find the method " %+% name %+% "."
			} else {
				"could not find the method " %+% dQuote(name) %+% ":\n" %+%
				"did you mean " %+% sample(similar, size = 1) %+% "?"
			}
		},
	must_be_named =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " %+% param %+%
			" must be a named collection."
		}
)



# -------------------------------- lament -------------------------------- #
#
# error messages for the forall testing function.
#

ddparse <- function (val, collapse = "") {
	paste0(deparse(val), collapse = collapse)
}
newline <- function (val) {
	paste0(val, collapse = "\n")
}

lament <- list(
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

