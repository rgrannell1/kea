
#' xReducer
#'
#' Successively combine a list of values into a single value
#' using a binary function (right to left).
#'
#' @param
#'    fn a binary function that returns a value
#'	  that \code{fn} can later take as its left argument.
#'
#' @param
#'    coll a collection.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'    Returns the empty list if \code{coll} is length-zero, and returns the
#'    value inside \code{coll} if coll is length-one.
#'
#' @family folding_functions
#'
#'
#' @template
#'    Variadic
#'
#' @template
#'    Return
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

	assert_is_fn_matchable(fn, invoking_call)

	assert_is_collection(coll, invoking_call)

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
				val <- try_higher_order(
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
