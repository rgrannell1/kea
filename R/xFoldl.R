
#' xFoldl
#'
#' Fold a function over a collection from left to right with an initial left value.
#'
#' @param
#'    fn a binary function that returns a value that
#'    \code{fn} can later take as its left argument.
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
#'    returns \code{init} if \code{coll} is length-zero.
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

xFoldl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the left

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(init), invoking_call,
		exclaim$parametre_missing(init))

	assert(
		!missing(coll), invoking_call,
		exclaim$parametre_missing(coll))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, profile_object(fn)) )

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, profile_object(coll)) )

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

			for (ith in seq_along(coll)) {

				init <- try_higher_order(
					fn( init, coll[[ith]] ),
					invoking_call)
			}
			init
		})
	}
}

#' @export

xFold <- xFoldl

#' @export

xFoldl... <- function (fn, init, ...) {
	xFoldl(fn, init, list(...))
}
