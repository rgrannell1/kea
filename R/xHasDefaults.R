
#' Return a boolean vector showing which parameters 
#'	 of f have default values.
#'
#' @param fn an arbitrary function, or a 
#'	 symbol or string identifying such a function.
#'
#' @return a list of true of false value of the same length as the 
#'	 arity of \code{fn}.
#'
#' @section Corner Cases:
#'	 if \code{fn} is a unary function return the empty list.
#' @family arrow-parameters
#' @export

#| function: xHasDefaults version: 0.1 finished: false

xHasDefaults <- function (fn) {
	# function -> Vector boolean
	# which of f's parameters have non-empty defaults?

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	fn <- match.fun(fn)

	if (xArity(fn) == 0) {
		logical(0)
	} else {
		unname(vapply(
			xFormals(fn),
			function (param) {
				!identical(param, quote(expr=))
			},
			True))		
	}
}
