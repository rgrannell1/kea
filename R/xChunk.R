
#' xChunk
#'
#' Divide a collection into segments of fixed length.
#'
#' @section Uses:
#'    \code{xChunk} is useful for reshaping a collection into pairs, triples,
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
#'    A list of lists.
#'
#' @section Corner Cases:
#'    The final list in the return value will have less than \code{num}
#'    elements if \code{length(coll)} is not evenly divisible by \code{num}.
#'    if \code{coll} is length-zero, the empty list is returned.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xChunk
#' @export

xChunk <- function (num, coll) {
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

	insist$must_be_length(num, 1)

	assert(
		num > 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, summate(num)) )

	insist$must_be_collection(coll, invoking_call)

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

#' @rdname xChunk
#' @export

xChunk... <- function (num, ...) {
	xChunk(num, list(...))
}
