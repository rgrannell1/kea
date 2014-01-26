
#' xFoldl
#'
#' Successively combine a list of values into a single value
#' using a binary function (left to right, with an initial value).
#'
#' @param
#'    fn a binary function that returns a value that
#'    \bold{fn} can later take as its left argument.
#'
#' @param
#'    val an arbitrary value. The initial value to be
#'    used as the first left argument to \bold{fn}.
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
#'    If \bold{coll} is length-zero then the parametre \bold{val}
#'    is returned automatically.
#'
#' @family folding_functions
#'
#' @template
#'    Variadic
#'
#' @template
#'    Fold
#'
#' @family short_circuiting_functions
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

	insist$must_be_fn_matchable(fn, invoking_call)
	insist$must_be_collection(coll, invoking_call)

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

				val <- try_hof(
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
