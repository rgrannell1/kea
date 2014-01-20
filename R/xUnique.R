
#' xUnique
#'
#' Return the unique elements in a collection.
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
#' @section Corner Cases:
#'    Returns the empty list if \code{colls} is length-zero.
#'
#' @family set_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xUnique
#' @export

xUnique <- function (coll) {
	# Collection any -> Collection any
	# remove duplicated values from a collection.

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert_is_collection(coll, invoking_call)

	if (length(coll) == 0) {
		list()
	} else {
		as.list(unique(coll))
	}
}

#' @rdname xUnique
#' @export

xUnique... <- function (...) {
	xUnique(list(...))
}
