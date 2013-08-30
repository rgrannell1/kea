
#' xWait
#' 
#' Delay the evalution of a function after invokation for a set amount of time/
#'
#' @param fn an arbitrary function.
#'
#' @return a function with the same parameters as \code{fn}.
#'
#' @section Corner Cases: 
#'     if \code{num} is zero then \code{fn} is returned untouched.
#' @template glossary
#'
#' @examples 
#' @export

xWait <- function (fn, num) {
	# function -> number -> function

	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a(c("positive double", "positive integer"), num, pcall)

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
