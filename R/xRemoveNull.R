
#' xRemoveNull
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
#'    inst/examples/example-xRemoveNull.R
#'
#' @rdname xRemoveNull
#' @export

xRemoveNull <- function (coll) {
	# Collection any -> [any]
	# remove the nan values from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		coll <- as.list(coll)
		coll[ vapply(coll, is.null, logical(1)) ] <- Null
		coll
	}
}

#' @rdname xRemoveNull
#' @export

xRemoveNull... <- function (...) {
	xRemoveNull(list(...))
}
