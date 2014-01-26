
#' xRemoveNan
#'
#' Remove the NaN values from a collection.
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
#'
#' @example
#'    inst/examples/example-xRemoveNan.R
#'
#' @rdname xRemoveNan
#' @export

xRemoveNan <- function (coll) {
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
		coll[is.nan(coll)] <- Null
		coll
	}
}

#' @rdname xPoll
#' @export

xRemoveNan... <- function (...) {
	xRemoveNan(list(...))
}
