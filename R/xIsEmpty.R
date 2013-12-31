
#' xIsEmpty
#'
#' Is a collection length-zero?
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    a boolean value.
#'
#' @family collection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xIsEmpty
#' @export

xIsEmpty <- function (coll) {
	# Collection -> boolean
	# is collection length == 0?

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	length(coll) == 0
}

#' @rdname xIsEmpty
#' @export

xIsEmpty... <- function (...) {
	xIsEmpty(list(...))
}
