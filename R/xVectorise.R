
#' xVectorise
#'
#' Convert a function on one value to a function on collections of values.
#'
#' @section Type Signature:
#'     (any -> any) -> (|any| -> [any])
#'
#' @details
#'    \bold{xVectorise} is a shorthand function for partially applying
#'    \bold{xMap} with a function, returning a vectorised function.
#'    This can be useful for taking a function that only works on single
#'    values - like \bold{is.null( )} - and extending them to work on
#'    collections of values.
#'
#'    \code{is.nulls <- xVectorise(is.null)}
#'
#'    \code{is.nulls(list(1, 2, Null, 4))}
#'
#'    \code{list(False, False, True, False)}
#'
#' @param
#'    fn a unary function. The function to vectorise.
#'
#' @return
#'    A unary function of coll.
#'
#' @section Corner Cases:
#'    Returns the empty list if \bold{coll} is length-zero.
#'
#' @family function_application_functions
#'
#' @family function_modifying_functions
#'
#' @example
#'    inst/examples/example-xVectorise.R
#'
#' @rdname xVectorise
#' @export

xVectorise <- MakeFun(function (fn) {

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	function (coll) {
		"a function created by xVectorise."
		""

		xMap(fn, coll)
	}
})

#' @rdname xVectorise
#' @export
# for my american friends.

xVectorize <- xVectorise
