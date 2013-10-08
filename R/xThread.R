
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
		!missing(init), pcall, 
		exclaim$parameter_missing(init))

	fns <- list(...)

	assert(all(sapply(fns, is_fn_matchable)), pcall)

	assert(
		all(sapply(fns, xArity) %in% c(1, Inf)), pcall)

	for (ith in seq_along(fns)) {

		init <- fns[[ith]]( init )
	}
	init
}
