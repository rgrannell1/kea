
#' xFoldl
#'
#' Fold a function over a collection from left to right with an initial left value.
#'
#' @param fn a binary function that returns a value that
#'	 \code{fn} can later take as its left argument.
#' @param init an arbitrary value.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'	 returns \code{init} if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xFoldl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the left"

	pcall <- sys.call()

	assert(
		!missing(fn), pcall,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), pcall,
		exclaim$parameter_missing(init))

	assert(
		!missing(coll), pcall,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	init <- dearrowise(init)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), pcall,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		init
	} else {

		callCC(function (Return) {

			clone_env <- new.env(parent = environment(fn))
			clone_env$Return <- Return

			environment(fn) <- clone_env

			for (ith in seq_along(coll)) {
				init <- fn( init, coll[[ith]] )
			}
			init
		})
	}
}

#' @export

xFold <- xFoldl
