
#' xTake
#' 
#' Take several elements from the head of a collection.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'	 If \code{coll} is empty the empty list is returned.
#' @template glossary
#'
#' @example inst/examples/blank.R
#'
#' @export

xTake <- function (num, coll) {
	# Collection any -> [any]
	# take the first num values of collection.

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

	num <- coerce_to_vector(num, 'numeric')

	assert(
		num >= 0, pcall,
		exclaim$must_be_greater_than(num, 0))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (length(coll) == 0 || num == 0) {
		list()
	} else {
		as.list(coll)[seq_len( min(num, length(coll)) )]
	}
}
