
#' xRepeat
#'
#' Repeat a collection several times.
#'
#' @param num a nonnegative positive number
#' @param coll a collection
#'
#' @return a list.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero or num is zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xRepeat <- function (num, coll) {
	# number -> Collection any -> [any]

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- coerce_to_typed_vector(num, "numeric", True)

	assert(
		length(num) %in% 0:1, pcall,
		exclaim$must_have_length(num, 0:1))

	assert(
		num >= 0, pcall,
		exclaim$must_be_grequal_than(num, 0))

	assert(
		round(num) == num, pcall,
		exclaim$must_be_whole(num))

	# a lot of assertions for a very simple function :/

	if (num == 0) {
		list()
	} else {
		rep(as.list(coll), num)
	}
}
