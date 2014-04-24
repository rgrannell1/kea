#' Enumerate all ways of choosing several elements from a collection.
#'
#' @details
#' \bold{xChoose} enumerates all ways of choosing \bold{num} distinct
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
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xChoose.R
#'
#' @rdname xChoose
#' @export

xChoose <- MakeFun(function (num, coll) {

	MACRO( Must $ Not_Be_Missing(num) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(num) )
	MACRO( Must $ Be_Collection(coll) )

	num <- unit_to_value(as_atom(num, 'numeric'))

	MACRO( Must $ Be_Whole(num) )
	MACRO( Must $ Be_Between(num, 0, Inf))

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
})

#' @rdname xChoose
#' @export

xChoose_ <- function (num, ...) {
	xChoose(num, list(...))
}
