
#' xInit
#'
#' Remove the first element from a collection.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a list.
#'
#' @family
#'    collection_functions
#'
#' @export

xInit <- function (coll) {
	# Collection any -> [any]
	# return everything but the first element of a
	# collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	coll <- dearrowise(coll)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}

#' @export

xInit... <- function (...) {
	xInit(list(...))
}
