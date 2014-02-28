
#' xChunk
#'
#' Divide a collection into segments of fixed length.
#'
#' @param
#'    num a nonnegative whole number. The desired
#'    length of each group of elements.
#'
#' @param
#'    coll a collection. The collections to divide into groups.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list of lists.
#'
#' @section Corner Cases:
#'    The final list in the return value will have less than \bold{num}
#'    elements if \code{length(coll)} is not evenly divisible by \bold{num}.
#'    if \bold{coll} is length-zero, the empty list is returned.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xChunk.R
#'
#' @rdname xChunk
#' @export

xChunk <- function (num, coll) {
	# integer -> Collection any -> [[any]]
	# groups coll into chunks of num,
	# when possible.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(num)
	insist $ must_not_be_missing(coll)

	insist $ must_be_collection(num, invoking_call)

	num <- unit_to_value(as_atom(num, 'numeric'))

	insist $ must_be_grequal_than(num, 0, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

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
