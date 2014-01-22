
#' xNotNull
#'
#' Is an element of a collection null?
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @section Corner Cases:
#'    Returns True if coll is Null.
#'
#' @template
#'    Variadic
#'
#' @rdname xNotNull
#' @export

xNotNull <- function (coll) {
	# collection any -> vector Boolean

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	if (length(coll) == 0 && is.null(coll)) {
		# empty pairlist.
		False
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- !identical(coll[[ith]], Null)
		}
		res
	}
}

#' @rdname xNotNull
#' @export

xNotNull... <- function (...) {
	xNotNull(list(...))
}
