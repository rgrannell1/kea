
#' xIsTrue
#'
#' Is an element of a collection true?
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A vector of boolean values.
#'
#' @template
#'    Variadic
#'
#' @rdname xIsTrue
#' @export

xIsTrue <- function (coll) {
	# Collection a -> Vector boolean
	# test which elements of a collection are true

	invoking_call <- sys.call()

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(coll, invoking_call)

	vapply(coll, function (x) {
		identical(x, True)
	}, logical(1), USE.NAMES = False)
}

#' @rdname xIsTrue
#' @export

xIsTrue... <- function (...) {
	xIsTrue(list(...))
}
