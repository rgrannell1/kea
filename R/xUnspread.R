
#' xUnspread
#'
#' Convert any function to a unary function.
#'
#' @details
#' \bold{xUnspread} takes a function and returns a function that
#' has one argument. The first element of the argument is
#' passed to the first parametre of the underlying function,
#' the second element to the second parametre, and so on.
#'
#' @param
#'    fn an arbitrary function. The function to be
#'    converted to a function that takes a single collection.
#'
#' @return
#'    A unary function of \bold{coll}, that applies its arguments
#'    to its underlying function.
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#' @family function_application_functions
#'
#' @example
#'    inst/examples/example-xUnspread.R
#'
#' @rdname xUnspread
#' @export

xUnspread <- MakeFun(function (fn) {
	# (... -> b) -> (a -> b)
	# dual to xSpread.
	# takes a function that takes a many values and
	# makes it into a function that takes one list.

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	function (coll) {
		"a function returned by xUnspread."
		""
		xApply(fn, coll)
	}
})
