
#' xJuxtapose
#' 
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @param ... a list or pairlist of functions.
#'
#' @return a variadic function.
#'
#' @section Corner Cases: 
#'    If no functions are provided the empty list is returned.
#'
#' @template glossary
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
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

	if (length(fns) == 0) {
		list()
	} else {
		function (...) {
			lapply(fns, function (fn) fn(...))
		}		
	}
}
