
#' xRepeat
#'
#' Repeat a collection several times.
#'
#' @param
#'    num a nonnegative positive number
#'
#' @param
#'    coll a collection
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero or num is zero.
#'
#' @family collection_functions
#'
#' @family reshaping_functions
#'
#' @family variadic_functions
#'
#' @rdname xRepeat
#' @export

xRepeat <- function (num, coll) {
	# number -> Collection any -> [any]

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	num <- as_typed_vector(num, "numeric", True)

	assert(
		length(num) == 1, invoking_call,
		exclaim$must_have_length(
			num, 1, profile_object(num)) )

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_grequal_than(
			num, 0, profile_object(num)) )

	assert(
		round(num) == num, invoking_call,
		exclaim$must_be_whole(
			num, profile_object(num)))

	# a lot of assertions for a very simple function :/

	if (num == 0) {
		list()
	} else {
		rep(as.list(coll), num)
	}
}

#' @rdname xRepeat
#' @export

xRepeat... <- function (num, ...) {
	xRepeat(num, list(...))
}
