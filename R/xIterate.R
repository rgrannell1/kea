
#' xIterate
#'
#' Iteratively apply a function to a value.
#'
#' @details
#'
#' \bold{xIterate} repeatedly calls a function on an initial value until
#' it is explicitely halted using \bold{Return( )}. It is similar
#' for a while loop, with the added benefits of an explicit return value
#' and easier debugging.
#'
#' The only way to end \bold{xIterate} is to call \bold{Return( )}. A
#' trivial example of ending an \bold{xIterate} call is given below.
#'
#' \code{xIterate(num := if (num >= 10) Return(num) else num + 1, 0)}
#'
#' \code{10}
#'
#' The above call iterates from zero to ten, before returning the
#' last number it encounters - ten. More useful examples are given below,
#' but the above example demonstrates the basic usage of \bold{xIterate}.
#'
#' @param
#'    fn a function. The function to repeatedly apply to an initial value.
#'
#' @param
#'    val an arbitrary value. The initial value to iteratively modify.
#'
#' @section Corner Cases:
#'    Length-zero values of \bold{val} are treated as normal values,
#'    since \bold{val} accepts an arbitrary value. Like while loops
#'    \bold{xIterate} is potentially non-terminating, and \bold{Return( )}
#'    must be called to terminate the function.
#'
#' @return
#'    The result of successively applying \bold{fn} to \bold{val}.
#'
#' @example
#'    inst/examples/example-xIterate.R
#'
#' @family short_circuiting_functions
#'
#' @rdname xIterate
#' @export

xIterate <- MakeFun(function (fn, val) {
	# (any -> any) -> any
	# iterate until higher-order returned.

	invoking_call <- sys.call()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(val) )

	MACRO( Must $ Be_Fn_Matchable(fn) )

	fn <- match_fn(fn)

	callCC(function (Return) {

		clone_env <- new.env(parent = environment(fn))
		clone_env$Return <- Return

		environment(fn) <- clone_env

		try_hof({repeat val <- fn(val)}, invoking_call)

	})
})
