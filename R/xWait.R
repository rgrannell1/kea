
#' xWait
#' 
#' Delay the evalution of a function after invokation for a set amount of time.
#'
#' @param fn an arbitrary function.
#' @param num a nonnegative whole number.
#'
#' @return a function with the same parameters as \code{fn}.
#'
#' @section Corner Cases: 
#'     if \code{num} is zero then \code{fn} is returned untouched.
#' @template glossary
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xWait <- function (fn, num) {
	# function -> number -> function

	pcall <- sys.call()
	
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))

	fn <- dearrowise(fn)
	num <- dearrowise(num)

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is.numeric(num) && num >= 0, pcall,
		exclaim$must_be_greater_than(num, 0))

	fn <- match.fun(fn)
	remove(pcall)

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
