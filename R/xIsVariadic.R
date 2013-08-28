
#' Is a function variadic?
#'
#' @param fn an arbitrary function, or a
#'	 symbol or string identifying such a function.
#'
#' @return a list of true of false value of the same length as the 
#'	 arity of \code{fn}.
#'
#' @section Corner Cases:
#'	 If \code{fn} is a nullary function the result is false.
#' @family arrow-parameters
#' @export

#| function: xIsVariadic version: 0.1 finished: true

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parameters?

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	f <- match.fun(fn)

	"..." %in% names(xFormals(fn))
}
