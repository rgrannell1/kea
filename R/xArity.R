
#' Return the number of parameters a function has.
#'
#' @param f an arbitrary function, or a 
#'     symbol or string identifying such a function.
#'
#' @return a positive whole number
#'
#' @section Corner Cases:
#'     If \code{f} is a variadic function of any kind
#'     positive infinity is returned.
#' @family arrow-parameters
#' @export

#| function: xArity version: 0.1 finished: true

xArity <- function (f) {
	# (a -> b) -> integer
	# get the arity of a function.
	# returns +Inf if f is a variadic function.

	pcall <- sys.call()
	require_a("functionable", f, pcall)
	
	f <- match.fun(f)
	f_params <- names(xFormals(f))

	if ("..." %in% f_params) {
		+Inf		
	} else {
		length(f_params)
	}
}