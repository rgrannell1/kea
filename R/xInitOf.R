
#' xInitOf
#'
#' Remove the last element of a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll a collection. A collection to return
#'    all but the last element of.
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
#'    inst/examples/example-xInitOf.R
#'
#' @rdname xInitOf
#' @export

xInitOf <- MakeFun('xInitOf', function (coll) {

	if (length(coll) < 2) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
})

#' @rdname xInitOf
#' @export

xInitOf_ <- MakeVariadic(xInitOf, 'coll')
