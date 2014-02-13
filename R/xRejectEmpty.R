
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

xRejectEmpty <- function (coll) {
	# Collection any -> [any]
	# remove the nan values from a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		coll[vapply(
			as.list(coll),
			length,
			integer(1)) == 0 ] <- Null

		coll
	}
}

#' @rdname xRejectEmpty
#' @export

xRejectEmpty... <- function (...) {
	xRejectEmpty(list(...))
}

#' @rdname xRejectEmpty
#' @export

xPack <- xRejectEmpty

#' @rdname xRejectEmpty
#' @export

xPack... <- xRejectEmpty...
