
#' xElemIsTrue
#'
#' Is an element of a collection true?
#'
#' @param
#'    coll a collection. The collection to test each element
#'    of for being true.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xElemIsTrue.R
#'
#' @family value_testing_functions
#'
#' @rdname xElemIsTrue
#' @export

xElemIsTrue <- MakeFun(function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Collection(coll) )

	vapply(coll, function (x) {
		identical(x, True)
	}, logical(1), USE.NAMES = False)
})

#' @rdname xElemIsTrue
#' @export

xElemIsTrue... <- function (...) {
	xElemIsTrue(list(...))
}
