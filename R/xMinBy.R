
#' xMinBy
#'
#' Get the smallest value in a collection according to a measure function.
#'
#' @param
#'     fn a unary function. The function to measure the size of
#'     an element.
#'
#' @param
#'    coll a non-empty collection. The collection to find the
#'    smallest value of.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An element from \bold{coll}.
#'
#' @section Corner Cases:
#'    If an empty collection is given, an error is thrown. This is because
#'    is impossible to return an element from a list that has none.
#'
#' @template
#'    Variadic
#'
#' @family selection_functions
#'
#' @example
#'    inst/examples/example-xMinBy.R
#'
#' @rdname xMinBy
#' @export

xMinBy <- MakeFun(function (fn, coll) {
	# (any -> number) -> Collection any -> any
	# get the smallest value in a collection by a metric.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )
	MACRO( Must $ Be_Longer_Than(0, coll) )

	fn <- match_fn(fn)

	coll[[ which.min( vapply(coll, fn, numeric(1)) ) ]]
})

#' @rdname xMinBy
#' @export

xMinBy... <- function (fn, ...) {
	xMinBy(fn, list(...))
}
