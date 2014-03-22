
#' xSortBy
#'
#' Sort a collection by the output of a function applied to each element.
#'
#' @details
#'     \bold{xSortBy} allows a collection to be sorted by a custom
#'     size operation. A classic example is sorting a collection of collections
#'     (analogous to a data frame) by a particular column.
#'
#'     \code{coll <- list(list('key1', 10), list('key2', 12), list('key3', 0))}
#'
#'     \code{xSortBy(xSecondOf, coll)}
#'
#'     \code{list(list("key2", 12), list("key1", 10), list("key3", 0))}
#'
#'     In the above example several rows of collection of collections are rearranged
#'     by the value in one of their columns.
#'
#' @param
#'    fn a function that returns a number. The size metric of an element.
#'
#' @param
#'    coll a collection. The collection to sort.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list
#'
#' @section Corner Cases:
#'    If \bold{coll} is a empty collection the empty list is returned.
#'
#' @template
#'    Variadic
#'
#' @family reshaping_functions
#'
#' @example
#'    inst/examples/example-xSortBy.R
#'
#' @rdname xSortBy
#' @export

xSortBy <- MakeFun(function (fn, coll) {
	#' (a -> b -> boolean) -> Collection any -> [any]
	#' sort a collection using a comparison function.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else if (length(coll) == 1) {
		as.list(coll)
	} else {
		as.list(coll)[ order( vapply(coll, fn, numeric(1)) ) ]
	}
})

#' @rdname xSortBy
#' @export

xSortBy... <- function (fn, ...) {
	xSortBy(fn, list(...))
}

