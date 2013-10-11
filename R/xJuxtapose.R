
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
#'
#' @family higher_order_function
#'
#' @examples inst/examples/blank.R
#' @export

xJuxtapose <- function (...) {
	# Recursive fns -> function

	pcall <- sys.call()

	fns <- list(...)

	assert(
		is.recursive(fns), pcall,
		exclaim$must_be_recursive)

	assert(all(sapply(fns, is_fn_matchable)), pcall,
		exclaim$must_be_recursive_of_matchable("..."))

	fns <- lapply(fns, match.fun)

	function (...) {
		lapply(fns, function (fn) fn(...))
	}
}
