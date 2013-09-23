
#' xThread
#' 
#' Iteratively apply a value to list of functions.
#'
#' @param init an arbitrary value
#' @param ... several unary functions.
#'
#' @return a list.
#'
#' @section Corner Cases: q
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xThread <- function (init, ...) {
	# any -> .... -> any
	# iteratively apply a value to each function in a list.

	pcall <- sys.call()

	assert(
		!missing(init), pcall)

	fns <- list(...)

	# are all fns functions/function names?
	assert(all(sapply(fns, function (fn) {
		is.function(fn) || is.symbol(fn) || (is.character(fn) && length(fn) == 1)
	}) ), pcall)

	assert(
		all(sapply(fns, xArity) %in% c(1, Inf)), pcall)

	for (ith in seq_along(fns)) {

		init <- fns[[ith]]( init )
	}
	init
}
