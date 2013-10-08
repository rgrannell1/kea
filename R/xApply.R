
#' xApply
#' 
#' Call a function with a list of arguments.
#'
#' @param fn an function of any arity.
#' @param coll a list or pairlist. This may be named, but
#'	all names must be parameters of \code{fn}.
#'
#' @return the return value of \code{fn}.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xApply <- function (fn, coll) {
	# function -> [any] -> any
	# call the function f with the list coll.

	pcall <- sys.call()
	pframe <- parent.frame()
	
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is.recursive(coll), pcall)

	fn <- match.fun(fn)

	eval(
		as.call(c(fn, coll)),
		envir = pframe)
}

#' @export

"%$%" <- xApply
