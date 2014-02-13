
#' xRejectNan
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
#'    inst/examples/example-xRejectNan.R
#'
#' @rdname xRejectNan
#' @export

xRejectNan <- function (coll) {
	# Collection any -> [any]
	# remove the nan values from a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

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

xRejectNan... <- function (...) {
	xRejectNan(list(...))
}
