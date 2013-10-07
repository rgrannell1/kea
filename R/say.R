 
exclaim <- list(
	parameter_missing = 
		function (param) {
			param <- paste(match.call()$param)

			"the parameter " + param + " is required but was missing."
		},
	must_be_matchable = 
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " + param + 
			" must be a function, or a symbol or string" + 
			" that can be looked-up as a function."
		},
	must_be_collection =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " + param +
			" must be a list, a pairlist or a typed vector."

		},
	must_be_unary = 
		function (param) {
			param <- paste(match.call()$param)

			"the function matching " + param +
			" must be a unary function."			
		},
	must_be_binary = 
		function (param) {
			param <- paste(match.call()$param)

			"the function matching " + param +
			" must be a binary function."			
		},
	must_be_recursive =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " + param +
			" must be a list or a pairlist."
		},
	must_be_poswhole_or_inf =
		function (param) {
			param <- paste(match.call()$param)

			"the argument matching " + param +
			" must be a positive whole number or positive infinite."

		},
	must_be_recursive_of_matchable = 
		function (param) {

			param <- paste(match.call()$param)

			"the arguments matching " + param + 
			" must all be functions, or symbols or strings" +
			" that can be looked-up as functions."

		},
	must_be_numeric =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " + param + 
			" must be a double or an integer."

		},
	must_be_character = 
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " + param + 
			" must be a character vector."

		},
	must_be_whole =
		function (param) {

			param <- paste(match.call()$param)

			"the argument matching " + param + 
			" must be a whole number."
		},
	must_have_length =
		function (param, length) {

			param <- paste(match.call()$param)
 
			"the argument matching " + param + 
			" must have length" + length

		},
	must_be_longer_than =
		function (param, length) {

			param <- paste(match.call()$param)

			"the argument matching " + param + 
			" must have length longer than " + length

		},
	must_be_larger_than =
		function (param, size) {

			param <- paste(match.call()$param)

			"the number matching " + param + 
			" must be larger than " + size

		}
)

