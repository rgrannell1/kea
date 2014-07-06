
#' xFold
#'
#' Successively combine a collection of values into a single value
#' using a binary function (left to right, with an initial value).
#'
#' @section Type Signature:
#'     (any -> any -> any) -> any -> |any| -> any
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

xFold <- MakeFun('xFold', function (fn, val, coll) {

	if (length(coll) == 0) {
		val
	} else {

		callCC(function (Return) {

			# -- can only use Return() in non-primitives
			if (!is.primitive(fn)) {
				clone_env <- new.env(parent = environment(fn))
				clone_env $ Return <- Return

				environment(fn) <- clone_env
			}

			for (ith in seq_along(coll)) {
				val <- MACRO( Try_Higher_Order_Function( fn( val, coll[[ith]] ) ) )
			}

			val
		})
	}
})


#' @export
#' @rdname xFold

xFold_ <- MakeVariadic(xFold, 'coll')
