
#' xChop
#'
#' Divide a collection into a fixed number of segments.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of \code{num} lists.
#'
#' @section Corner Cases:
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xChop
#' @export

xChop <- function (num, coll) {
	# integer -> Collection any -> [[any]]
	# chop a collection into several collections.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) == 1, invoking_call,
		exclaim$must_have_length(
			num, 1, summate(num)) )

	assert(
		num > 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, summate(num)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	if (length(coll) == 0) {
		list()
	} else if (is.infinite(num)) {
		as.list(coll)
	} else {

		ith <- current <-1
		average_elems <- ceiling(length(coll) / num)

		chopped <- list()

		while (current <= length(coll)) {

			to_select <- min(length(coll), (current + average_elems - 1))

			chopped[[ith]] <-
				as.list(coll[current : to_select])

			ith <- ith + 1
			current <- current + average_elems
		}

		chopped
	}
}

#' @rdname xChop
#' @export

xChop... <- function (num, ...) {
	xChop(num, list(...))
}
