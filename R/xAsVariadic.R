
#' Return a variadic function that passes its arguments as a list to its underlying function.
#'
#' @param fn a unary function, or a 
#'     symbol or string identifying such a function.
#' @return a variadic function.
#'
#' @export

#| function: xAsVariadic version: 0.1 finished: false 

xAsVariadic <- function (fn) {
	# (a -> b) -> (... -> b)
	# Return a variadic function that passes its arguments 
	# as a list to its underlying function.
	
	pcall <- sys.call()
	require_a("functionable", fn, pcall)

	fn <- match.fun(fn)
	require_a("unary function", fn, pcall)

	function (...) {
		fn(list(...))
	}
}