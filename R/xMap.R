
#' xMap
#'
#' Apply a function to each element of a collection.
#'
#' @param
#'    fn a unary function. The function to modify each
#'    element of a collection with.
#'
#' @param
#'    coll a collection. The collection to be modified.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xMap.R
#'
#' @rdname xMap
#' @export

xMap <- MakeFun(function (fn, coll) {
	# (any -> any) -> Collection any -> [any]
	# map a unary function over a collection x.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		try_hof(
			lapply(coll, fn), invoking_call)
	}
})

#' @rdname xMap
#' @export

xMap... <- function (fn, ...) {
	xMap(fn, list(...))
}
