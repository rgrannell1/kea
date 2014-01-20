
#' xFlatten
#'
#' Unnest a nested list to a given depth.
#'
#' @details
#'    \code{num} specifies the maximum allowed depth in the
#'    returned list;
#'
#' @param
#'    num a nonnegative whole-number.
#'
#' @param
#'    coll a list or pairlist.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero.
#'    Flattening to infinite doesn't affect the depth of the output list.
#'    Flattening to one level flattens the list fully.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xFlatten
#' @export

xFlatten <- function (num, coll) {
	# integer -> Collection any-> [any]
	# flatten a collection to an arbitrary depth.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		num > 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, summate(num)) )

	assert(
		round(num) == num, invoking_call,
		exclaim$must_be_whole(
			num, summate(num)) )

	assert_is_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else if (is.atomic(coll)) {
		as.list(coll)
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

#' @rdname xFlatten
#' @export

xFlatten... <- function (num, ...) {
	xFlatten(num, list(...))
}