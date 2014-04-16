
#' xIsMember
#'
#' Check if a collection contains a value.
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
#'    Various types of \code{Na} are not-distinguished between.
#'    Type conversion is not carried out.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xIsMember.R
#'
#' @rdname xIsMember
#' @export

xIsMember <- MakeFun(function (val, coll) {
	# Collection any -> any -> Vector logical
	# check if a collection contains a value.

	MACRO( Must $ Not_Be_Missing(val) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		logical(0)
	} else {

		for (elem in coll) {
			if ( isTRUE(identical(elem, val, single.NA = True)) ) {
				return(True)
			}
		}

		False
	}
})

#' @rdname xIsMember
#' @export

xIsMember... <- function (val, ...) {
	xIsMember(val, list(...))
}
