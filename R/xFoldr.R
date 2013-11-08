#'
#' Fold a function over a collection from right to length, with an init value.
#'
#' @param fn a binary function that returns a value that
#'	 \code{fn} can later take as its right argument
#' @param init an arbitrary value.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'	 returns \code{init} if \code{coll} is length-zero.
#'
#' @template Return
#'
#' @family higher_order_functions collection_functions
#'
#' @export

xFoldr <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), parent_call,
		exclaim$parameter_missing(init))

	assert(
		!missing(coll), parent_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	init <- dearrowise(init)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), parent_call,
		exclaim$must_be_collection(coll))

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		init
	} else {

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

#' @export

xFoldr... <- function (fn, init, ...) {
	xFoldr(fn, init, list(...))
}
