
#' Return a unary function that passes each element of its arguments as a parameter to 
#'    its underlying function.
#'
#' @param fn an arbitrary function, or a 
#'     symbol or string identifying such a function.
#' @return a unary function with a parameter 'args'.
#'
#' @export
#'

#| function: xAsUnary version: 0.1 finished: false 

xAsUnary <- function (fn) {
	# (... -> b) -> (a -> b)
	# dual to xMakeVariadic. 
	# takes a function that takes a many values and 
	# makes it into a function that takes one list.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)

	fn <- match.fun(fn)
	remove(pcall)

	function (args) {
		xApply(fn, as.list(args))
	}
}
