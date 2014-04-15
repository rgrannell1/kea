
#' xRejectNull
#'
#' Remove the null values from a collection.
#'
#' @param
#'    coll a collection. The collection to remove
#'    null values from.
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
#'    inst/examples/example-xRejectNull.R
#'
#' @rdname xRejectNull
#' @export

xRejectNull <- MakeFun(function (coll) {
	# Collection any -> [any]
	# remove the nan values from a collection.

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {
		coll <- as.list(coll)
		coll[ vapply(coll, is.null, logical(1)) ] <- Null
		coll
	}
})

#' @rdname xRejectNull
#' @export

xRejectNull... <- function (...) {
	xRejectNull(list(...))
}
