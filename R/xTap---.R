
#' $ xTap
#'
#' Apply an anonymous function to the contents of an arrow object.
#'
#' @section Type Signature:
#'     (any -> any) -> any -> any
#'
#' @param fn a unary function. The function to apply to the data in an arrow object.
#'
#' @details
#'    \bold{xTap} applies non-arrow functions to be applied to arrow objects.
#'    These functions can be named or unnamed. This allows base functions
#'    or external libraries to interoperate with arrow's chaining methods.
#'
#'    \code{x_(letters) $ xShuffle() $ x_Tap(sort)}
#'
#'    \bold{xTap} also allows the use of anonymous methods.
#'
#'    \code{x_(1:10) $ x_Tap(nums := all(nums > 0))}
#'
#' @usage
#'      x_(  ) $ xTap(fn)
#'
#' @return
#'      An arrow object.
#'
#' @family methods
#'
#' @name xTap

xTap <- MakeFun(function (fn, val) {

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(val) )

	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	fn(val)
})
