
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
#' @example inst/examples/blank.R
#' @export

xFlatten <- function (num, coll) {
	# integer -> Collection any-> [any]
	# flatten a collection to an arbitrary depth.

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))
	
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- coerce_to_vector(num, 'numeric')

	assert(
		num > 0, pcall,
		exclaim$must_be_greater_than(num, 0))

	assert(
		round(num) == num, pcall,
		exclaim$must_be_whole(num))

	assert(
		is.recursive(coll), pcall,
		exclaim$must_be_recursive(coll))

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
