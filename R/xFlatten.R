
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
#'
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xFlatten <- function (num, coll) {
	# integer -> Collection any-> [any]
	# flatten a collection to an arbitrary depth.

	parent_call <- sys.call()

	assert(
		!missing(num), parent_call,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		num > 0, parent_call,
		exclaim$must_be_greater_than(num, 0))

	assert(
		round(num) == num, parent_call,
		exclaim$must_be_whole(num))

	assert(
		is_recursive(coll), parent_call,
		exclaim$must_be_recursive(coll))

	if (length(coll) == 0) {
		list()
	} else if (num == +Inf) {
		as.list(coll)
	} else if (num == 1) {
		as.list(unlist(coll))
	} else {

		recur <- function (depth, xs) {
			if (!is_recursive(xs)) {
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

#' @export

xFlatten... <- function (num, ...) {
	xFlatten(num, list(...))
}