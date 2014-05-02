
#' xRejectNan
#'
#' Remove the NaN values from a collection.
#'
#' @section Type Signature:
#'     |any| -> [any]
#'
#' @param
#'    coll a collection. The collection to remove
#'    NaN values from.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @template
#'    Variadic
#'
#' @family filtering_functions
#'
#' @example
#'    inst/examples/example-xRejectNan.R
#'
#' @rdname xRejectNan
#' @export

xRejectNan <- MakeFun(function (coll) {

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {
		# must be list to be able to replace with NULL
		coll <- as.list(coll)
		coll[vapply(coll, function (elem) {
			is.atomic(elem) && isTRUE(is.nan(elem))
		}, logical(1), USE.NAMES = False) ] <- Null
		coll
	}
})

#' @rdname xRejectNan
#' @export

xRejectNan_ <- MakeVariadic(xRejectNan, 'coll')
