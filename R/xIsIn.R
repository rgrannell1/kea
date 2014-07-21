
#' xIsIn
#'
#' Check if a collection contains a value.
#'
#' @section Type Signature:
#'     any -> |any| -> &lt;logical>
#'
#' @param
#'    val an arbitrary value. The value to test for membership
#'    in \bold{coll}.
#'
#' @param
#'    coll a collection. The collection of elements to check against
#'    \bold{val}.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
#'
#' @section Corner Cases:
#'    Various types of \bold{Na} are not-distinguished between.
#'    Type conversion is not carried out.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsIn.R
#'
#' @rdname xIsIn
#' @export

xIsIn <- MakeFun('xIsIn', function (val, coll) {

	if (length(coll) == 0) {
		logical(0)
	} else {

		for (elem in coll) {
			if ( isTRUE(identical(elem, val)) ) {
				return(True)
			}
		}

		False
	}
})

#' @rdname xIsIn
#' @export

xIsIn_ <- MakeVariadic(xIsIn, 'coll')
