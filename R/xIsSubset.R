
#' xIsSubset
#'
#' Test if a collection is a subset of a second collection.
#'
#' @param
#'    coll1 a collection. The set to test for membership in a superset.
#'
#' @param
#'    coll1 a collection. The superset to test.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    Returns logical(0) if \bold{coll1} or \bold{coll2} is length-zero.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsSubset.R
#'
#' @family set_functions
#'
#' @rdname xIsSubset
#' @export

xIsSubset <- MakeFun(function (coll1, coll2) {
	# Collection any -> Collection any -> boolean
	# Is a collection a subset of another collection.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(coll1) )
	MACRO( Must $ Not_Be_Missing(coll2) )

	MACRO( Must $ Be_Collection(coll1) )
	MACRO( Must $ Be_Collection(coll2) )

	if (length(coll1) == 0 || length(coll2) == 0) {
		logical(0)
	} else {

		for (elem in coll1) {
			if (elem %!in% coll2) {
				return(False)
			}
		}
		True
	}
})

#' @rdname xIsSubset
#' @export

xIsSubset... <- function (coll1, ...) {
	xIsSubset(coll1, list(...))
}