
#' xSegment
#'
#' Divide a collection into segments of fixed length.
#'
#' @section Uses:
#' \code{xSegment} is useful for reshaping a collection into pairs, triples,
#' or larger groups before applying a function to each group.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of n-element lists.
#'
#' @section Corner Cases:
#'	 the final list in the return value will have less than \code{num}
#'	 elements if \code{length(coll)} is not evenly divisible by \code{num}.
#'	 if \code{coll} is length-zero, the empty list is returned.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xSegment <- function (num, coll) {
	# integer -> Collection any -> [[any]]
	# groups coll into chunks of num,
	# when possible.

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	assert(
		length(num) == 1, pcall,
		exclaim$must_have_length(num, 1))

	num <- coerce_to_typed_vector(num, 'numeric')

	assert(
		num >= 0, pcall,
		exclaim$must_be_grequal_than(num, 0))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0) {
		list()
	} else {
		lapply(
			seq(1, to = length(coll), by = num),
			function (lower) {
				as.list(coll[ lower:min(length(coll), lower + num - 1) ])
		})
	}
}
