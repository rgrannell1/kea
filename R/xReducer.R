
#' xReducer
#'
#' Fold a function over a collection from right to left.
#'
#' @param fn a binary function that returns a value
#'	 that \code{fn} can later take as its left argument.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'     returns the empty list if \code{coll} is length-zero, and returns the
#'     value inside \code{coll} if coll is length-one.
#'
#' @family higher_order_functions
#'
#' @template Return
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xReducer <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {

		init <- coll[[ length(coll) ]]
		coll <- xInit(coll)

		callCC(function (Return) {

			clone_env <- new.env(parent = environment(fn))
			clone_env$Return <- Return

			environment(fn) <- clone_env

			for (ith in length(coll):1) {
				init <- fn( coll[[ith]], init )
			}
			init
		})
	}
}