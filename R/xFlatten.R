
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
#'    Returns the empty list if \bold{coll} or \bold{num} is length-zero.
#'    Flattening to infinite doesn't affect the depth of the output list.
#'    Flattening to one level flattens the list fully. Flattening removes all
#'   list-names.
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

xFlatten <- MakeFun('xFlatten', function (num, coll) {

	MACRO( Must_Be_Between(num, 1, Inf))
	MACRO( Must_Be_Whole(num) )

	if (length(coll) == 0 || length(num) == 0) {

		list()

	} else if (is_atomic(coll)) {

		# -- it is flat!
		unname(as.list(coll))

	} else if (num == +Inf) {

		# -- preserve the structure.
		unname(as.list(coll))

	} else if (num == 1) {

		# -- unlist entirely.
		unname(rapply(coll, identity, how = 'list'))

	} else {

		recur <- function (depth, xs) {
			if (!is_recursive(xs)) {
				xs
			} else if (depth == num - 1) {
				unname(as.list(unlist(coll)))
			} else {
				unname( lapply(xs, function (x) recur(depth + 1, x)) )
			}
		}
		as.list(recur(0, coll))

	}
})

#' @rdname xFlatten
#' @export

xFlatten_ <- MakeVariadic(xFlatten, 'coll')
