
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

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		lapply(coll, fn)
	}
})

#' @rdname xMap
#' @export

xMap_ <- MakeFun(function (fn, ...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xMap(fn, list(...))
})
