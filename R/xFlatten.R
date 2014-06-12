
#' xFlatten
#'
#' Unnest a nested list to a given depth.
#'
#' @section Type Signature:
#'    |numeric| -> |any| -> |any|
#'
#' @details
#'    \bold{xFlatten} flattens nested collections to a
#'    specific depth. \bold{num} specifies the maximum
#'    allowed depth in the returned list. When flattening a
#'    (infinitely nested) collection, \bold{num} equals 1 returns a
#'    collection, \bold{num} equals 2 returns a collection of
#'    collections, and so on.
#'
#'    \bold{xFlatten} is currently implemented recursively (except for
#'    complete unlisting), so it is unstable for large or deep collections.
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


	num <- unit_to_value(as_atom(num, 'numeric'))

	MACRO( Must $ Be_Between(num, 1, Inf))
	MACRO( Must $ Be_Whole(num) )

	if (length(coll) == 0) {
		list()
	} else if (is_atomic(coll)) {
		# -- it is flat!
		as.list(coll)
	} else if (num == +Inf) {
		# -- preserve the structure.
		as.list(coll)
	} else if (num == 1) {
		# -- unlist entirely.
		as.list(unlist(coll))
	} else {

		# -- todo write this non-recursively!
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

xFlatten_ <- MakeVariadic(xFlatten, 'coll')
