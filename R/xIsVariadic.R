
#' xIsVariadic
#' 
#' Is a function variadic?
#'
#' @param fn an arbitrary function or primitive function.
#'
#' @return a boolean value.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parameters?

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	"..." %in% names(xFormals(fn))

}
