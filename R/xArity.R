
#' Return the number of parameters a function has.
#'
#' @param fn an arbitrary function, or a 
#'     symbol or string identifying such a function.
#'
#' @return a positive whole number
#'
#' @section Corner Cases:
#'     If \code{fn} is a variadic function of any kind
#'     positive infinity is returned.
#' @family arrow-parameters
#' @export

#| function: xArity version: 0.1 finished: true

xArity <- function (fn) {
	# (a -> b) -> integer
	# get the arity of a function.
	# returns +Inf if fn is a variadic function.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	
	fn <- match.fun(fn)
	fn_params <- names(xFormals(fn))

	if ("..." %in% fn_params) {
		+Inf		
	} else {
		length(fn_params)
	}
}