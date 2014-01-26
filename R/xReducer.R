
#' xReducer
#'
#' Successively combine a list of values into a single value
#' using a binary function (right to left).
#'
#' @param
#'    fn a binary function that returns a value
#'	  that \bold{fn} can later take as its left argument.
#'
#' @param
#'    coll a collection. The collection to reduce to a
#'    single value.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An arbitrary value, depending on the function \bold{fn}.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero, and returns the
#'    value inside \bold{coll} if coll is length-one.
#'
#' @family folding_functions
#'
#' @template
#'    Fold
#'
#' @template
#'    Variadic
#'
#' @family short_circuiting_functions
#'
#' @example
#'    inst/examples/example-xReducer.R
#'
#' @rdname xReducer
#' @export

xReducer <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.

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
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {

		val <- coll[[ length(coll) ]]
		coll <- xInitOf(coll)

		callCC(function (Return) {

			if (!is.primitive(fn)) {
				clone_env <- new.env(parent = environment(fn))
				clone_env$Return <- Return

				environment(fn) <- clone_env
			}

			for (ith in length(coll):1) {
				val <- try_hof(
					fn( coll[[ith]], val ),
					invoking_call)
			}
			val
		})
	}
}

#' @rdname xReducer
#' @export

xReducer... <- function (fn, ...) {
	xReduce(fn, list(...))
}

xReduce... <- xReducer...
