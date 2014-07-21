
#' xIterate
#'
#' Iteratively apply a function to a value.
#'
#' @section Type Signature:
#'    (any -> any) -> any -> any
#'
#' @details
#'
#'     \bold{xIterate} repeatedly calls a function on an initial value until
#'     it is explicitely halted using \bold{Return( )}. It is similar
#'     for a while loop, with the added benefits of an explicit return value
#'     and easier debugging.
#'
#'     The only way to end \bold{xIterate} is to call \bold{Return( )}. A
#'     trivial example of ending an \bold{xIterate} call is given below.
#'
#'     \code{xIterate(num := if (num >= 10) Return(num) else num + 1, 0)}
#'
#'     \code{10}
#'
#'     The above call iterates from zero to ten, before returning the
#'     last number it encounters - ten. More useful examples are given below,
#'     but the above example demonstrates the basic usage of \bold{xIterate}.
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

xIterate <- MakeFun('xIterate', function (fn, val) {

	MACRO( Must_Have_Arity(fn, 1) )

	callCC(function (Return) {

		# -- assign the Return( ) function into an anonymous function.
		clone_env <- new.env(parent = environment(fn))

		clone_env $ Return <- Return
		environment(fn)    <- clone_env

		repeat val <- MACRO( Try_Higher_Order_Function( fn(val) ) )
	})
})
