
#' xFoldr
#'
#' Successively combine a list of values into a single value
#' using a binary function (right to left, with an initial value).
#'
#' @param
#'    fn a binary function that returns a value that
#'	  \bold{fn} can later take as its right argument
#'
#' @param
#'    val an arbitrary value. The initial value to be
#'    used as the first right argument to \bold{fn}.
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
#' @family short_circuiting_functions
#'
#' @template
#'    Fold
#'
#' @template
#'    Variadic
#'
#' @template
#'    Return
#'
#' @example
#'    inst/examples/example-xFoldr.R
#'
#' @rdname xFoldr
#' @export

xFoldr <- MakeFun(function (fn, val, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )
	MACRO( arrow ::: Must $ Not_Be_Missing(val) )
	MACRO( arrow ::: Must $ Not_Be_Missing(coll) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )
	MACRO( arrow ::: Must $ Be_Collection(coll) )

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

			try_hof({
				for (ith in length(coll):1) {
					val <- fn( coll[[ith]], val )
				}},
				invoking_call
			)

			val
		})

	}
})

#' @rdname xFoldr
#' @export

xFoldr... <- function (fn, val, ...) {
	xFoldr(fn, val, list(...))
}
