
#' xScanl
#'
#' Successively combine a list of values into a single value
#' using a binary function (left to right, with an initial value).
#' Return a list containing each intermediate result, and the final result.
#'
#' @param
#'    fn a binary function that returns a value that
#'    \bold{fn} can later take as its right argument.
#'
#' @param
#'    val an arbitrary value. The initial value to be
#'    used as the first left argument to \bold{fn}.
#'
#' @param
#'    coll a collection. The collection to successively
#'    reduce to a single value.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A list with its initial element being \bold{coll}, and
#'	  containing \bold{length(coll) + 1}.
#'
#' @section Corner Cases:
#'	  Returns \bold{list(val)} if \bold{coll} is length-zero.
#'
#' @family folding_functions
#' @family short_circuiting_functions
#'
#' @template
#'    Fold
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xScanl.R
#'
#' @rdname xScan
#' @export

xScanl <- function (fn, val, coll) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	insist $ must_be_fn_matchable(fn, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

	fn <- match_fn(fn)

	scanned <- c( val, vector("list", length(coll)) )

	if (length(coll) == 0) {
		val
	} else {
		for (ith in seq_along(coll)) {
			scanned[[ith + 1]] <- try_hof(
				fn( scanned[[ith]], coll[[ith]] ),
				invoking_call)
		}
		scanned
	}
}

#' @rdname xScan
#' @export

xScan <- xScanl

#' @rdname xScan
#' @export

xScanl... <- function (fn, val, ...) {
	xScanl(fn, val, list(...))
}

#' @rdname xScan
#' @export

xScan... <- function (fn, val, ...) {
	xScan(fn, val, list(...))
}
