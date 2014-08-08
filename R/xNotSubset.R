
#' xNotSubset
#'
#' Test if a collection is not a subset of a second collection.
#'
#' @section Type Signature:
#'     |any| -> |any| -> &lt;logical>
#'
#' @param
#'    coll1 a collection. The set to test for non-membership in a superset.
#'
#' @param
#'    coll2 a collection. The superset to test.
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
#'    inst/examples/example-xNotSubset.R
#'
#' @family set_functions
#'
#' @rdname xNotSubset
#' @export

xNotSubset <- MakeFun('xNotSubset', function (coll1, coll2) {

	if (length(coll1) == 0 || length(coll2) == 0) {
		logical(0)
	} else {

		for (elem1 in coll1) {
			# -- 'in' is needed here.

			has_match <- False

			for (elem2 in coll2)  {
				if (identical(elem1, elem2)) {
					has_match <- True
				}
			}

			if (!has_match) {
				return (True)
			}
		}
		False
	}
})

#' @rdname xNotSubset
#' @export

xNotSubset_ <- MakeVariadic(xNotSubset, 'coll2')
