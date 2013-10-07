
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
#' @examples inst/examples/blank.R
#' @export

xAsVariadic <- function (fn) {
	# (a -> b) -> (... -> b)
	# Return a variadic function that passes its arguments 
	# as a list to its underlying function.
	
	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	
	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(1, Inf), pcall)

	function (...) {
		fn(list(...))
	}
}