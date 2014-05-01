#' Remove the first element from a collection.
#'
#' @param
#'    coll a collection. The collection to subset.
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
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRestOf.R
#'
#' @rdname xRestOf
#' @export

xRestOf <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
})

#' @rdname xRestOf
#' @export

xRestOf_ <- MakeVariadic(xRestOf, 'coll')
