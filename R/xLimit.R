
#' xLimit
#' 
#' Create a function that can call its underlying function a limited number of times.
#'
#' @param num a positive whole number.
#' @param fn an arbitrary function.
#'
#' @return a function with the same parameters as \code{fn}.
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xLimit <- function (num, fn) {
	# integer -> function -> function

	pcall <- sys.call()

	assert(
		!missing(num), pcall,
		exclaim$parameter_missing(num))
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		length(num) == 1, pcall)
	assert(
		is.numeric(num), pcall)
	assert(
		num > 0, pcall)

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	.count <- 0

	do.call('function', list(
		as.pairlist(xFormals(fn)),
		bquote(

			if (.count < num) {
				.count <<- .count + 1
				.( call_with_params('fn', fn) )
			} else NULL )
	))
}
