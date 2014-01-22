
#' xTake
#'
#' Take several elements from the head of a collection.
#'
#' @param
#'      num a nonnegative whole number.
#'
#' @param
#'    ... see above.
#'
#' @param
#'      coll a collection
#'
#' @return
#'      A list.
#'
#' @section Corner Cases:
#'    If \code{coll} is empty the empty list is returned.
#'
#' @family selection_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xTake
#' @export

xTake <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_collection(num, invoking_call)
	num <- as_typed_vector(num, 'numeric', True)

	insist$must_be_of_length(num, 1)
	insist$must_be_grequal_than(num, 0)
	insist$must_be_whole(num, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	if (length(coll) == 0 || num == 0) {
		list()
	} else {
		as.list(coll)[seq_len( min(num, length(coll)) )]
	}
}

#' @rdname xTake
#' @export

xTake... <- function (num, ...) {
	xTake(num, list(...))
}
