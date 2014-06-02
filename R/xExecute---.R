
#' $ xExecute
#'
#' Execute a (possibly side-effectful) function before
#' continuing with the previous method result.
#'
#' @usage
#'      x_(  ) $ xExecute(fn)
#'
#' @param
#'      fn a nullary function
#'
#' @return
#'      The return value of the previous method.
#'
#' @family methods
#'
#' @family inpure_functions
#'
#' @name xExecute

xExecute <- MakeFun(function (fn, val) {

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(val) )

	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	fn()
	val
})
