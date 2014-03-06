
#' xFirstOf
#'
#' Return the first element of a collection.
#'
#' @param
#'    coll a collection. The collection to get the
#'    first element of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The first element in \bold{coll}.
#'
#' @section Corner Cases:
#'    Throws an error if \bold{coll} has less than one element; this is
#'    because any other corner case would violate the functions type-signature.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xFirstOf.R
#'
#' @rdname xFirstOf
#' @export

xFirstOf <- MakeFun(function (coll) {
	# Collection any -> any
	# return the first element of a collection x.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )
	MACRO( Must $ Be_Longer_Than(0, coll) )

	coll[[1]]
})

#' @rdname xFirstOf
#' @export

xFirstOf... <- function (...) {
	xFirstOf(list(...))
}
