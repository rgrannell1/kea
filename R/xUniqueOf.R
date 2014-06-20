
#' xUniqueOf
#'
#' Return the unique elements in a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll a collection. The collection to return unique
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
#'    inst/examples/example-xUniqueOf.R
#'
#' @rdname xUniqueOf
#' @export

xUniqueOf <- MakeFun('xUniqueOf', function (coll) {

	if (length(coll) == 0) {
		list()
	} else {
		as.list(unique(coll))
	}
})

#' @rdname xUniqueOf
#' @export

xUniqueOf_ <- MakeVariadic(xUniqueOf, 'coll')
