
#' xSortBy
#'
#' Sort a collection by the output of a function applied to each element.
#'
#' @section Type Signature:
#'    (any -> &lt;number>) -> |any| -> |any|
#'
#' @details
#'     \bold{xSortBy} allows a collection to be sorted by a custom
#'     measure of size. A typical use is sorting a collection of collections
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
#'    fn a function that returns a number. The measure of an elements size.
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
#'    If \bold{coll} is a empty collection the empty list is returned. Throws an error if
#'    \bold{fn} returns Na or NaN or a non length-one-numeric value.
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

xSortBy <- MakeFun('xSortBy', function (fn, coll) {

	if (length(coll) == 0) {
		keep_names(list(), coll)
	} else if (length(coll) == 1) {
		as.list(coll)
	} else {
		# -- for readable error messages
		fn_applied_to_coll <- vapply(coll, fn, numeric(1))

		MACRO( Must_Not_Contain_Na(fn_applied_to_coll) )
		MACRO( Must_Not_Contain_Nan(fn_applied_to_coll) )

		# -- TODO test for na values?

		as.list(coll)[order(fn_applied_to_coll)]
	}
})

#' @rdname xSortBy
#' @export

xSortBy_ <- MakeVariadic(xSortBy, 'coll')
