
#' xFold
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
#'    inst/examples/example-xFold.R
#'
#' @rdname xFold
#' @export

xFold <- MakeFun(function (fn, val, coll) {

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(val) )
	MACRO( Must $ Not_Be_Missing(coll) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(coll) )

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
				val <- fn( val, coll[[ith]] )
			}

			val
		})
	}
})


#' @export
#' @rdname xFold

xFold_ <- MakeFun(function (fn, val, ...) {

	MACRO( Must $ Have_Canonical_Arguments() )

	xFold(fn, val, list(...))
})
