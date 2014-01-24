
#' xIsEmpty
#'
#' Is a collection length-zero?
#'
#' @param
#'    coll a collection. The collection to check.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A boolean value.
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

	insist$must_be_collection(coll, invoking_call)

	length(coll) == 0
}

#' @rdname xIsEmpty
#' @export

xIsEmpty... <- function (...) {
	xIsEmpty(list(...))
}
