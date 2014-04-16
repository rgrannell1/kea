
#' xNotMember
#'
#' Check if a collection doesn't contain a value.
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
#'    inst/examples/example-xNotMember.R
#'
#' @rdname xNotMember
#' @export

xNotMember <- MakeFun(function (val, coll) {
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
				return(False)
			}
		}

		True
	}
})

#' @rdname xNotMember
#' @export

xNotMember... <- function (val, ...) {
	xNotMember(val, list(...))
}
