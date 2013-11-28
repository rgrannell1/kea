
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
#'
#'
#' @family collection_functions
#'
#' @example inst/examples/blank.R
#' @export

xRepeat <- function (num, coll) {
	# number -> Collection any -> [any]

	parent_call <- sys.call()

	assert(
		!missing(num), parent_call,
		exclaim$parameter_missing(num))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	num <- dearrowise(num)
	coll <- dearrowise(coll)

	num <- as_typed_vector(num, "numeric", True)

	assert(
		length(num) == 1, parent_call,
		exclaim$must_have_length(num, 1))

	assert(
		num >= 0, parent_call,
		exclaim$must_be_grequal_than(num, 0))

	assert(
		round(num) == num, parent_call,
		exclaim$must_be_whole(num))

	# a lot of assertions for a very simple function :/

	if (num == 0) {
		list()
	} else {
		rep(as.list(coll), num)
	}
}

#' @export

xRepeat... <- function (num, ...) {
	xRepeat(num, list(...))
}
