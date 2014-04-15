
#' xSecondOf
#'
#' Return the second element in a collection.
#'
#' @param
#'    coll a collection. The collection to get the
#'    second element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The second element in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than two
#'    elements; this is because any other corner case
#'    would violate the functions type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xSecondOf.R
#'
#' @rdname xSecondOf
#' @export

xSecondOf <- MakeFun(function (coll) {
	# Collection any -> any
	# return the second element of a collection x.

	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )
	MACRO( Must $ Be_Longer_Than(1, coll) )

	coll[[2]]
})

#' @rdname xSecondOf
#' @export

xSecondOf... <- function (...) {
	xSecondOf(list(...))
}
