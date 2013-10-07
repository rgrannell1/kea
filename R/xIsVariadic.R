
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
#' @examples inst/examples/blank.R
#' @export

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parameters?

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	"..." %in% names(xFormals(fn))

}
