
#' xIsNull
#'
#' Is an element of a collection null?
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns True if coll is Null.
#'
#' @family collection_functions
#'
#' @family variadic_functions
#'
#' @export

xIsNull <- function (coll) {
	# collection any -> vector Boolean

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 && is.null(coll)) {
		# empty pairlist.
		True
	} else {
		res <- vector(mode = 'logical', length(coll))

		for (ith in seq_along(coll)) {
			res[ith] <- identical(coll[[ith]], Null)
		}
		res
	}
}

#' @export

xIsNull... <- function (...) {
	xIsNull(list(...))
}
