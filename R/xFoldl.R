
#' xFoldl
#'
#' Successively combine a list of values into a single value
#' using a binary function (left to right, with an initial value).
#'
#' @param
#'    fn a binary function that returns a value that
#'    \code{fn} can later take as its left argument.
#'
#' @param
#'    val an arbitrary value.
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
#'    returns \code{val} if \code{coll} is length-zero.
#'
#' @family folding_functions
#'
#' @template
#'    Variadic
#'
#' @template
#'    Return
#'
#' @example
#'    inst/examples/example-xFoldl.R
#'
#' @rdname xFoldl
#' @export

xFoldl <- function (fn, val, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the left

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

	assert_is_fn_matchable(fn, invoking_call)

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(
			coll, summate(coll)) )

	fn <- match_fn(fn)

	if (length(coll) == 0) {
		val
	} else {

		callCC(function (Return) {

			if (!is.primitive(fn)) {
				clone_env <- new.env(parent = environment(fn))
				clone_env$Return <- Return

				environment(fn) <- clone_env
			}

			for (ith in seq_along(coll)) {

				val <- try_higher_order(
					fn( val, coll[[ith]] ),
					invoking_call)
			}
			val
		})
	}
}

#' @export
#' @rdname xFoldl

xFold <- xFoldl

#' @export
#' @rdname xFoldl

xFoldl... <- function (fn, val, ...) {
	xFoldl(fn, val, list(...))
}
