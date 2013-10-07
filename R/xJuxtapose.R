
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
#' @export

xJuxtapose <- function (...) {
	# Recursive fns -> function

	pcall <- sys.call()

	fns <- list(...)

	assert(
		is.recursive(fns), pcall,
		exclaim$must_be_recursive)

	# is every element a function or function name?
	assert(all( sapply(fns, function (fn) {
			is_fn_matchable(fn)
		}) ), pcall,
		exclaim$must_be_recursive_of_matchable("...")
	)

	fns <- lapply(fns, match.fun)

	function (...) {
		lapply(fns, function (fn) fn(...))
	}
}
