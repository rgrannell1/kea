
#' xArity
#' 
#' Return the arity of a function.
#'
#' @param fn an function of any arity.
#'
#' @return a positive whole number.
#'
#' @section Corner Cases:
#'	 If \code{fn} is a variadic function of any kind
#'	 positive infinity is returned.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xArity <- function (fn) {
	# Function -> integer
	# get the arity of a function.
	# returns +Inf if fn is a variadic function.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))
	
	fn <- match.fun(fn)
	fn_params <- names(xFormals(fn))

	if ("..." %in% fn_params) {
		+Inf		
	} else {
		length(fn_params)
	}
}