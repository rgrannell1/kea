
#' xWait
#'
#' Delay the evalution of a function after invokation
#' for a set amount of time.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @return
#'    a function with the same parameters as \code{fn}.
#'
#' @section Corner Cases:
#'    if \code{num} is zero then \code{fn} is returned untouched.
#'
#' @family higher_order_functions
#'
#' @export

xWait <- function (fn, num) {
	# function -> number -> function

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(num), invoking_call,
		exclaim$parameter_missing(num))

	fn <- dearrowise(fn)
	num <- dearrowise(num)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is.numeric(num) && num >= 0, invoking_call,
		exclaim$must_be_greater_than(num, 0))

	fn <- match.fun(fn)
	remove(invoking_call)

	if (num == 0) {
		fn
	} else {
		do.call("function", list(
			formals(fn),
			bquote({
				Sys.sleep(num)
				.( call_with_params("fn", fn) )
			})
		))
	}
}
