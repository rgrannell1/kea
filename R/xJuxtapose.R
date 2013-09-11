
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
#' @examples 
#' @export

xJuxtapose <- function (...) {
	# Recursive fns -> function

	pcall <- sys.call()

	fns <- list(...)
	require_a('recursive_of_functionable', fns, pcall)

	fns <- lapply(fns, match.fun)

	function (...) {
		lapply(fns, function (fn) fn(...))
	}
}
