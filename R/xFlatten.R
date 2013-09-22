
#' xFlatten
#' 
#' Flatten a nested list or pairlist.
#'
#' @param num a nonnegative whole-number.
#' @param coll a list or pairlist.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples 
#' @export

xFlatten <- function (num, coll) {
	# integer -> Collection any-> [any]
	# flatten a collection to an arbitrary depth.

	pcall <- sys.call()

	assert(
		!missing(num), pcall)
	assert(
		!missing(coll), pcall)

	assert(
		is.numeric(num) || is.infinite(num) && num > 0, pcall)

	assert(
		is.list(coll) || is.pairlist(coll), pcall)

	if (length(coll) == 0) {
		list()
	} else if (num == +Inf) {
		as.list(coll)
	} else if (num == 1) {
		as.list(unlist(coll))
	} else {

		recur <- function (depth, xs) {
			if (!is.recursive(xs)) {
				xs
			} else if (depth == num - 1) {
				as.list(unlist(xs))
			} else {
				lapply(xs, function (x) recur(depth + 1, x))
			}
		}
		as.list(recur(0, coll))		
	}
}
