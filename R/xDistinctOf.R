
#' xDistinctOf
#'
#' Return the distinct elements in a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll a collection. The collection to return distinct
#'    elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{colls} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDistinctOf.R
#'
#' @rdname xDistinctOf
#' @export

xDistinctOf <- MakeFun(function (coll) {

	# C++ is too slow at the moment.

	if (length(coll) == 0) {
		list()
	} else {
		as.list(unique(coll))
	}

	cDistinctOf(coll)
})

#' @rdname xDistinctOf
#' @export

xDistinctOf_ <- MakeVariadic(xDistinctOf, 'coll')
