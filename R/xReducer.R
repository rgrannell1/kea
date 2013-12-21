
#' xReducer
#'
#' Fold a function over a collection from right to left.
#'
#' @param
#'    fn a binary function that returns a value
#'	  that \code{fn} can later take as its left argument.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'    returns the empty list if \code{coll} is length-zero, and returns the
#'    value inside \code{coll} if coll is length-one.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    collection_functions
#'
#' @template
#'    Return
#'
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

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, profile_object(collfn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {

		init <- coll[[ length(coll) ]]
		coll <- xInit(coll)

		callCC(function (Return) {

			if (!is.primitive(fn)) {
				clone_env <- new.env(parent = environment(fn))
				clone_env$Return <- Return

				environment(fn) <- clone_env
			}

			for (ith in length(coll):1) {
				init <- try_higher_order(
					fn( coll[[ith]], init ),
					invoking_call)
			}
			init
		})
	}
}

#' @export

xReducer... <- function (fn, ...) {
	xReduce(fn, list(...))
}

xReduce... <- xReducer...
