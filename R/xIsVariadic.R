
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
#' @examples 
#' @export

xIsVariadic <- function (fn) {
	# function -> Vector boolean
	# is ... in fn's parameters?

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	f <- match.fun(fn)

	"..." %in% names(xFormals(fn))

}
