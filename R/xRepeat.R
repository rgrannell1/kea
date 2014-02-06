
#' xRepeat
#'
#' Repeat a collection several times.
#'
#' @param
#'    num a nonnegative positive number. The number of
#'    times to repeat the collection.
#'
#' @param
#'    coll a collection. The collection to repeat
#'    end to end.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero or num is zero.
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
	# repeat a collection several times.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

    insist $ must_be_collection(num, invoking_call)

	num <- unit_to_value(as_typed_vector(num, "numeric"))

	insist $ must_be_of_length(num, 1, invoking_call)
	insist $ must_be_grequal_than(num, 0, invoking_call)
	insist $ must_be_whole(num, invoking_call)

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
