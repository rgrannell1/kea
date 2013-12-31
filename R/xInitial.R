
#' xInit
#'
#' Remove the first element from a collection.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xInit
#' @export

xInit <- function (coll) {
	# Collection any -> [any]
	# return everything but the first element of a
	# collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	if (length(coll) == 0 || length(coll) == 1) {
		list()
	} else {
		coll <- as.list(coll)
		coll[-length(coll)]
	}
}

#' @rdname xInit
#' @export

xInit... <- function (...) {
	xInit(list(...))
}
