
#' xUnspread
#'
#' Convert any function to a unary function.
#'
#' @section Type Signature:
#'     (...any -> any) -> (any -> any)
#'
#' @details
#'     \bold{xUnspread} takes a function and returns a function that
#'     has one argument. The first element of the argument is
#'     passed to the first parametre of the underlying function,
#'     the second element to the second parametre, and so on.
#'
#'     \bold{xUnspread} adapts a multi-parametre function to work
#'     as a unary function; for example in functions like \bold{xMap}.
#'
#' @param
#'     fn an arbitrary function. The function to be
#'     converted to a function that takes a single collection.
#'
#' @return
#'     A unary function of \bold{coll}, that applies its arguments
#'     to its underlying function.
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

	fn <- match_fn(fn)

	function (coll) {
		"a function returned by xUnspread."
		""
		xApply(fn, coll)
	}
})
