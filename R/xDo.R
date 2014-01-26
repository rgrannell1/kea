
#' xDo
#'
#' Map (a possibly side-effectful) function over a
#' collection and discard the results.
#'
#' @section Uses:
#'
#' @param
#'    fn a unary function, usually side-effectful.
#'
#' @param
#'    coll a collection
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list.
#'
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xDo.R
#'
#' @rdname xDo
#' @export

xDo <- function (fn, coll) {
	# function -> Collection any -> Null
	# apply a function to each element of a collection.
	# and discard the results.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist$must_be_fn_matchable(fn, invoking_call)
	insist$must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		list()
	} else {
		for (ith in seq_along(coll)) {
			try_hof(
				fn( coll[[ith]] ), invoking_call)
		}

		invisible (Null)
	}
}

#' @rdname xDo
#' @export

xDo... <- function (fn, ...) {
	xDo(fn, list(...))
}
