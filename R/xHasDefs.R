
#' xHasDefs
#' 
#' Return a boolean vector showing which parameters of a function has defaults.
#'
#' @param fn an arbitrary function.
#'
#' @return a named vector of true of false value of the same length as the 
#'	 arity of \code{fn}.
#'
#' @section Corner Cases:
#'     if \code{fn} is nullary \code{logical(0)} is returned.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xHasDefs <- function (fn) {
	# function -> named Vector boolean
	# which of f's parameters have non-empty defaults?

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	if (xArity(fn) == 0) {
		logical(0)
	} else {
		vapply(
			xFormals(fn),
			function (param) {
				!identical(param, quote(expr=))
			},
			logical(1))		
	}
}
