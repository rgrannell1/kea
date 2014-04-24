
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

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {
		# must be list to be able to replace with NULL
		coll <- as.list(coll)
		coll[ vapply(coll, function (elem) isTRUE(is.null(elem)), logical(1)) ] <- Null
		coll
	}
})

#' @rdname xRejectNull
#' @export

xRejectNull_ <- function (...) {
	xRejectNull(list(...))
}
