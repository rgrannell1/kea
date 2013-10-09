
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
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	function (...) {
		fn(list(...))
	}
}