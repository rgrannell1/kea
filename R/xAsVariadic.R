
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

	assert(
		!missing(fn), pcall)
	
	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(1, Inf), pcall)

	function (...) {
		fn(list(...))
	}
}