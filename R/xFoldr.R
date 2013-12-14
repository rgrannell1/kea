
#' xFoldr
#'
#' Fold a function over a collection from right to length, with an init value.
#'
#' @param
#'    fn a binary function that returns a value that
#'	  \code{fn} can later take as its right argument
#'
#' @param
#'    init an arbitrary value.
#'
#' @param
#'    coll a collection.
#'
#' @return
#'    an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'	  returns \code{init} if \code{coll} is length-zero.
#'
#' @family
#'    higher_order_functions
#'
#' @family
#'    collection_functions
#'
#' @template Return
#'
#' @export

xFoldr <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), invoking_call,
		exclaim$parameter_missing(init))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	Object()



	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		init
	} else {

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

xFoldr... <- function (fn, init, ...) {
	xFoldr(fn, init, list(...))
}
