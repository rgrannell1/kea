
#' xRestOf
#'
#' Remove the first element from a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
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
#'    Returns the empty list if \bold{coll} is length-zero
#'    or length-one.
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

xRestOf <- MakeFun('xRestOf', function (coll) {

	if (length(coll) < 2) {
		list()
	} else {
		as.list( coll[-1] )
	}
})

#' @rdname xRestOf
#' @export

xRestOf_ <- MakeVariadic(xRestOf, 'coll')
