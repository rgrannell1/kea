
#' xNotIn
#'
#' Check if a collection doesn't contain a value.
#'
#' @section Type Signature:
#'     any -> |any| -> &lt;logical>
#'
#' @param
#'    val an arbitrary value. The value to test for
#'    non-membership in a collection.
#'
#' @param
#'    coll a collection. The collection to test elements from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    Various types of \code{Na} are not-distinguished between.
#'    Type conversion is not carried out.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xNotIn.R
#'
#' @rdname xNotIn
#' @export

xNotIn <- MakeFun('xNotIn', function (val, coll) {

	if (length(coll) == 0) {
		logical(0)
	} else {

		for (elem in coll) {
			if ( isTRUE(identical(elem, val)) ) {
				return(False)
			}
		}

		True
	}
})

#' @rdname xNotIn
#' @export

xNotIn_ <- MakeVariadic(xNotIn, 'coll')
