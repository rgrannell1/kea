
#' xRecurMap
#'
#' Recursively map a function into a nested collection, preserving its structure.
#' 
#' @param fn a unary function.
#' @param coll a list or pairlist.
#'
#' @return a list or pairlist.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xRecurMap <- function (fn, coll) {
	# (any -> any) -> List|Pairlist any -> [any]
	# Map a function into a nested collection, 
	# preserving its structure.	

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))
	
	assert(
		is.list(coll) || is.pairlist(coll), pcall)

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(1, Inf), pcall)

	recur <- function (xs) {

		if (is.list(xs) || is.pairlist(xs)) {
			lapply(xs, recur)
		} else {
			fn(xs)
		}
	}
	recur(as.list(coll))
}
