
#' xRemoveEmpty
#'
#' Remove the null values from a collection.
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
#' @rdname xRemoveEmpty
#' @export

xRemoveEmpty <- function (coll) {
	# Collection any -> [any]
	# remove the nan values from a collection.

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
		coll[vapply(
			as.list(coll),
			length,
			integer(1)) == 0 ] <- Null

		coll
	}
}

#' @rdname xRemoveEmpty
#' @export

xRemoveEmpty... <- function (...) {
	xRemoveEmpty(list(...))
}


