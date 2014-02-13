
#' xCombos
#'
#' Enumerate all ways of choosing several elements from a collection.
#'
#' @details
#' \bold{xCombos} enumerates all ways of choosing \bold{num} distinct
#' elements from a larger collection, where order doesn't matter.
#'
#' The number of ways of choosing \bold{num} elements from a collection
#' is given by the base R function \code{choose(num, length(coll))}.
#'
#' @param
#'    num a nonnegative whole number. The number of elements to choose
#'    from \bold{coll}.
#'
#' @param
#'    coll a collection. The collection to draw elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of lists, with each list containing \bold{num} elements.
#'
#' @section Corner Cases:
#'      Returns the empty list if \bold{num} is zero.
#'
#' @family combinatoric_functions
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xCombos.R
#'
#' @rdname xCombos
#' @export

xCombos <- function (num, coll) {
	# number -> Collection anyf
	# generate all ways of choosing several
	# elements from a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(num)

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(num, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	num <- unit_to_value(as_atom(num, 'numeric'))

	insist $ must_be_whole(num, invoking_call)
	insist $ must_be_grequal_than(num, 0, invoking_call)

	if (num == 0) {
		list()
	} else if (length(coll) == 0) {
		list()
	} else {

		if (is.pairlist(coll)) {
			coll <- as.list(coll)
		}

		num <- min(length(coll), num)
		apply(combn(coll, num), 2, as.list)
	}
}

#' @rdname xCombos
#' @export

xCombos... <- function (num, ...) {
	xCombos(num, list(...))
}
