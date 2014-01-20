
#' xRemoveNa
#'
#' Remove the NA values from a collection.
#'
#' @param
#'    coll a collection.
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
#' @rdname xRemoveNa
#' @export

xRemoveNa <- function (coll) {
	# Collection any -> [any]
	# remove the na values from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	if (length(coll) == 0) {
		list()
	} else {
		coll <- as.list(coll)
		coll[is.na(coll)] <- Null
		coll
	}
}

#' @rdname xPoll
#' @export

xRemoveNa... <- function (...) {
	xRemoveNa(list(...))
}
