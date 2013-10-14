
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
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
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

	num <- coerce_to_vector(num, 'numeric')

	assert(
		length(num) == 1, pcall,
		exclaim$must_have_length(num, 1))

	assert(
		num > 0, pcall,
		exclaim$must_be_whole(num))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	.count <- 0

	do.call('function', list(
		as.pairlist(xFormals(fn)),
		bquote(

			if (.count < num) {
				.count <<- .count + 1
				.( call_with_params('fn', fn) )
			} else Null )
	))
}
