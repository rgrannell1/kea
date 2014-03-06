
#' xLenOf
#'
#' Get the length of a collection.
#'
#' @param
#'    coll a collection. The collection to test the
#'    length of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A nonnegative integer.
#'
#' @section Corner Cases:
#'      Returns zero if \bold{coll} is empty.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xLenOf.R
#'
#' @rdname xLenOf
#' @export

xLenOf <- MakeFun(function (coll) {
	# Collection a -> integer
	# get the length of a collection.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Collection(coll) )

	length(coll)
})

#' @rdname xLenOf
#' @export

xLenOf... <- function (...) {
	xLenOf(list(...))
}
