
#' xRejectNa
#'
#' Remove the Na values from a collection.
#'
#' @param
#'    coll a collection. The collection to remove
#'    na values from.
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
#'    inst/examples/example-xRejectNa.R
#'
#' @rdname xRejectNa
#' @export

xRejectNa <- function (coll) {
	# Collection any -> [any]
	# remove the na values from a collection.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		coll <- as.list(coll)
		coll[is.na(coll)] <- Null
		coll
	}
}

#' @rdname xRejectNa
#' @export

xRejectNa... <- function (...) {
	xRejectNa(list(...))
}
