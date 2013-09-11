
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
#' @examples 
#' @export

xRecurMap <- function (fn, coll) {
	# (any -> any) -> List|Pairlist any -> [any]
	# Map a function into a nested collection, 
	# preserving its structure.	

	pcall <- sys.call()
	require_a(traits$functionable, fn, pcall)
	require_a("recursive", coll, pcall)

	fn <- match.fun(fn)
	require_a("unary function", fn, pcall)

	recur <- function (xs) {

		if (is.list(xs) || is.pairlist(xs)) {
			lapply(xs, recur)
		} else {
			fn(xs)
		}
	}
	recur(as.list(coll))
}
