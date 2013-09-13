
#' xThread
#' 
#' Iteratively apply a value to list of functions.
#'
#' @param init an arbitrary value
#' @param ... several unary functions.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xThread <- function (init, ...) {
	# any -> .... -> any

	pcall <- sys.call()

	fns <- list(...)

	# are all fns functions/function names?
	assert(all(sapply(fns, function (fn) {
		is.function(fn) || is.symbol(fn) || (is.character(fn) && length(fn) == 1)
	}) ), pcall)

	assert(
		all(sapply(fns, xArity) %in% c(1, Inf)), pcall)

	ith <- 1
	while (ith <= length(fns)) {

		init <- fns[[ith]]( init )
		ith <- ith + 1
	}
	init
}

#' @export

'=>' <- xThread
