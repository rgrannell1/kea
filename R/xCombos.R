
#' xCombos
#'
#' Generate all ways of choosing several elements from a collection.
#'
#' @param num a nonnegative whole number.
#' @param coll a collection
#'
#' @return a list of lists, with each list containing \code{num} elements.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{num} is zero.
#'
#'
#' @family combinatoric_functions collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xCombos <- function (num, coll) {
	# number -> Collection any
	# generate all ways of choosing several
	# elements from a collection.

	parent_call <- sys.call()

	assert(
		!missing(num), parent_call,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- coerce_to_typed_vector(num,  'numeric', True)

	assert(
		length(num) %in% 0:1, parent_call,
		exclaim$must_have_length(num, 0:1))

	assert(
		num >= 0, parent_call,
		exclaim$must_be_greater_than(num, 0))
	assert(
		round(num) == num, parent_call,
		exclaim$must_be_whole(num))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	if (num == 0) {
		list()
	} else {
		num <- min(length(coll), num)
		apply(combn(coll, num), 2, as.list)
	}
}

#' @export

xCombos... <- function (num, ...) {
	xCombos(num, list(...))
}
