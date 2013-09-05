
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
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xFlatten <- function (num, coll) {
	# integer -> Collection any-> [any]
	# flatten a collection to an arbitrary depth.

	pcall <- sys.call()

	require_a(c('positive whole', 'positive infinite'), num, pcall)
	require_a('recursive', coll, pcall)

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
