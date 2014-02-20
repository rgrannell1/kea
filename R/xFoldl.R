
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

	insist $ must_not_be_missing(fn)
	insist $ must_not_be_missing(val)
	insist $ must_not_be_missing(coll)

	insist $ must_be_fn_matchable(fn, invoking_call)
	insist $ must_be_collection(coll, invoking_call)

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

			report <- progress_bar(length(coll), invoking_call)

			try_hof({
				for (ith in seq_along(coll)) {

					report(ith)

					val <- fn( val, coll[[ith]] )
				}},
				invoking_call
			)

			report(length(coll), True)

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
