
#' xGroup
#'
#' Divide a collection into segments of fixed length.
#'
#' @section Uses:
#'    \code{xGroup} is useful for reshaping a collection into pairs, triples,
#'    or larger groups before applying a function to each group.
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
#'    A list of n-element lists.
#'
#' @section Corner Cases:
#'    the final list in the return value will have less than \code{num}
#'    elements if \code{length(coll)} is not evenly divisible by \code{num}.
#'    if \code{coll} is length-zero, the empty list is returned.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xGroup
#' @export

xGroup <- function (num, coll) {
	# integer -> Collection any -> [[any]]
	# groups coll into chunks of num,
	# when possible.

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
		list(as.list(coll))
	} else {
		lapply(
			seq(1, to = length(coll), by = num),
			function (lower) {
				as.list(coll[ lower:min(length(coll), lower + num - 1) ])
		})
	}
}

#' @rdname xGroup
#' @export

xGroup... <- function (num, ...) {
	xGroup(num, list(...))
}
