
#' xFlatten
#'
#' Unnest a nested list to a given depth.
#'
#' @details
#'    \bold{xFlatten} flattens nested collections to a
#'    specific depth. \bold{num} specifies the maximum
#'    allowed depth in the returned list. When flattening a
#'    (infinitely nested) collection, \bold{num} equals 1 returns a
#'    collection, \bold{num} equals 2 returns a collection of
#'    collections, and so on.
#'
#' @param
#'    num a nonnegative whole-number. The maximum depth
#'    of collection in the return value.
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
#'    Returns the empty list if \bold{coll} is length-zero.
#'    Flattening to infinite doesn't affect the depth of the output list.
#'    Flattening to one level flattens the list fully.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xFlatten.R
#'
#' @rdname xFlatten
#' @export

xFlatten <- MakeFun(function (num, coll) {
	# integer -> Collection any-> [any]
	# flatten a collection to an arbitrary depth.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(num) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(num) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	num <- unit_to_value(as_atom(num, 'numeric'))

	insist $ must_be_greater_than(num, 0, invoking_call)
	MACRO( arrow ::: Must $ Be_Whole(num) )

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
})

#' @rdname xFlatten
#' @export

xFlatten... <- function (num, ...) {
	xFlatten(num, list(...))
}
