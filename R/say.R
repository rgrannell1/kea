 
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
			" must be a function, or a symbol or string that can be looked-up as a function."
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
		}
)
