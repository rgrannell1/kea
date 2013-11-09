
#' xIsVariadic
#'
#' Is a function variadic?
#'
#' @param fn an arbitrary function or primitive function.
#'
#' @return a boolean value.
#'
#'
#'
#' @example inst/examples/blank.R
#' @export

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parameters?

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	"..." %in% names(xFormals(fn))

}
