
#' xSpread
#'
#' Return a variadic function that passes its arguments
#' as a list to its underyling function.
#'
#' @section Type Signature:
#'    ([any] -> any) -> (...any -> any)
#'
#' @details
#'    \code{xSpread} takes a unary function and
#'    returns a function with ellipsis parametres.
#'    The returned function passes its ellipsis arguments
#'    as a single list to the underlying function.
#'
#' @param
#'    fn a unary function. The function to convert
#'    to a variadic function.
#'
#' @return
#'    A variadic function.
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#' @family function_application_functions
#'
#' @example
#'    inst/examples/example-xSpread.R
#'
#' @rdname xSpread
#' @export

xSpread <- MakeFun(function (fn) {

	fn <- match_fn(fn)

	function (...) {
		"a function returned by xSpread."
		""
		fn(list(...))
	}
})
