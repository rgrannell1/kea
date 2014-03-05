
#' xAsUnary
#'
#' Convert any function to a unary function.
#'
#' @details
#' \bold{xAsUnary} takes a function and returns a function that
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
#'    inst/examples/example-xAsUnary.R
#'
#' @rdname xAsUnary
#' @export

xAsUnary <- MakeFun(function (fn) {
	# (... -> b) -> (a -> b)
	# dual to xAsVariadic.
	# takes a function that takes a many values and
	# makes it into a function that takes one list.

	invoking_call <- sys.call()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	function (coll) {
		"a function returned by xAsUnary."
		""
		xApply(fn, coll)
	}
})
