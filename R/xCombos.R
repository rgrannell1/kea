
#' xCombos
#'
#' Generate all ways of choosing several elements from a collection.
#'
#' @section Uses:
#' \code{xCombos} .
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of lists, with each list containing \code{num} elements.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{num} is zero.
#' @template glossary
#'
#' @family combinatoric_functions
#'
#' @example inst/examples/blank.R
#' @export

xCombos <- function (num, coll) {
	# number -> Collection any
	# generate all ways of choosing several
	# elements from a collection.

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- coerce_to_typed_vector(num,  'numeric')

	assert(
		length(num) == 1, pcall,
		exclaim$must_have_length(num, 1))

	assert(
		num >= 0, pcall,
		exclaim$must_be_greater_than(num, 0))

	assert(
		round(num) == num, pcall,
		exclaim$must_be_whole(num))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	if (num == 0 || length(coll) == 0) {
		list()
	} else {
		num <- min(length(coll), num)
		apply(combn(coll, num), 2, as.list)
	}
}
