
#' xAsVariadic
#' 
#' Return a variadic function that passes its arguments as a list to ints underyling function.
#'
#' @param fn a unary function.
#'
#' @return a variadic function.
#'
#' @template glossary
#'
#' @examples 
#' @export

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