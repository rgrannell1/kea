
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
#' @examples 
#' @export

xLimit <- function (num, fn) {
	# integer -> function -> function

	pcall <- sys.call()
	require_a('positive whole', num, pcall)
	require_a(traits$functionable, fn, pcall)

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
