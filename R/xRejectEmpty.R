
#' xRejectEmpty
#'
#' Remove the empty values from a collection.
#'
#' @param
#'    coll a collection. The collection to remove
#'    length-zero values from.
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
#'    inst/examples/example-xRejectEmpty.R
#'
#' @rdname xRejectEmpty
#' @export

xRejectEmpty <- MakeFun(function (coll) {
	# Collection any -> [any]
	# remove the nan values from a collection.

	MACRO( Must $ Not_Be_Missing(coll) )
	MACRO( Must $ Be_Collection(coll) )

	if (length(coll) == 0) {
		list()
	} else {
		# must be list to be able to replace with NULL
		coll <- as.list(coll)
		# vectorise the comparison step
		coll[vapply(coll, length, integer(1)) == 0 ] <- Null

		coll
	}
})

#' @rdname xRejectEmpty
#' @export

xRejectEmpty_ <- function (...) {
	xRejectEmpty(list(...))
}

#' @rdname xRejectEmpty
#' @export

xPack <- xRejectEmpty

#' @rdname xRejectEmpty
#' @export

xPack_ <- xRejectEmpty_
