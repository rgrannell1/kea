
#' xUniqueOf
#'
#' Return the unique elements in a collection.
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

xUniqueOf <- MakeFun(function (coll) {
	# Collection any -> Collection any
	# remove duplicated values from a collection.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {
		as.list(unique(coll))
	}
})

#' @rdname xUniqueOf
#' @export

xUniqueOf... <- function (...) {
	xUniqueOf(list(...))
}
