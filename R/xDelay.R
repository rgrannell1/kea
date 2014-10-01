
#' xDelay
#'
#' Delay the evaluation of a function after invocation
#' for a set amount of time.
#'
#' @section Type Signature:
#'     function -> |number| -> function
#'
#' @details
#'    \bold{xDelay} is primarily meant for use with side-effectual functions;
#'    likely examples including functions that call web API's and functions that
#'    that display graphics.
#'
#'    \bold{xDelay} throttles the rate at which a function can be repeatedly
#'    executed. This can be useful in preventing API rate limits from being
#'    exceeded or to emulate a refresh rate.
#'
#' @param
#'    fn an arbitrary function. The function to slow the rate of execution of.
#'
#' @param
#'    num a nonnegative whole number. The number of seconds
#'    to delay execution by.
#'
#' @return
#'    A function with the same parametres as \bold{fn}.
#'
#' @section Corner Cases:
#'    If \bold{num} is zero then \bold{fn} is returned untouched.
#'
#' @template S-Uncertain
#' @family function_modifying_functions
#'
#' @family time_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xDelay.R
#'
#' @rdname xDelay
#' @export

xDelay <- MakeFun(function (fn, num) {

	MACRO( Must_Be_Longer_Than(0, num) )
	MACRO( Must_Be_Between(num, 0, Inf))

	if (num == 0) {
		fn
	} else {

		do.call("function", list(
			as.pairlist(xFormalsOf(fn)),
			bquote({
				"A function created by xDelay."
				""
				Sys.sleep(num)
				.( call_with_params("fn", fn) )
			})
		))
	}
})
