
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
#' @param
#'    ... see above.
#'
#' @return
#'    a list.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero or num is zero.
#'
#' @family reshaping_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xRepeat.R
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
			num, 1, summate(num)) )

	assert(
		num >= 0, invoking_call,
		exclaim$must_be_grequal_than(
			num, 0, summate(num)) )

	assert(
		round(num) == num, invoking_call,
		exclaim$must_be_whole(
			num, summate(num)))

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
