
#' xJuxtapose
#' 
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @param fns a list or pairlist of functions.
#'
#' @return a variadic function.
#'
#' @section Corner Cases: 
#'
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R
#' @examples inst/examples/blank.R 
#' @export

xJuxtapose <- function (...) {
	# Recursive fns -> function

	pcall <- sys.call()

	fns <- list(...)

	assert(is.recursive(fns))

	# is every element a function or function name?
	assert(all( sapply(fns, function (fn) {
		is.function(fn) || is.symbol(fn) || (is.character(fn) && length(fn) == 1)
	}) ), pcall)

	fns <- lapply(fns, match.fun)

	function (...) {
		lapply(fns, function (fn) fn(...))
	}
}
