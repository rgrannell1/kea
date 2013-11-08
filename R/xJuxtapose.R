
#' xJuxtapose
#'
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @param fns a list or pairlist of functions.
#'
#' @return a variadic function.
#'
#' @section Corner Cases:
#'    If no functions are provided the empty list is returned.
#'
#'
#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xJuxtapose <- function (fns) {
	# Recursive fns -> function

	parent_call <- sys.call()

	fns <- lapply(list(fns), dearrowise)

	assert(
		is.recursive(fns), parent_call,
		exclaim$must_be_recursive(fns))

	assert(
		all(sapply(fns, is_fn_matchable)), parent_call,
		exclaim$must_be_recursive_of_matchable("..."))

	fns <- lapply(fns, match_fn)

	if (length(fns) == 0) {
		list()
	} else {
		function (...) {
			lapply(fns, function (fn) fn(...))
		}
	}
}

#' @export

xJuxtapose... <- function (...) {
	xJuxtapose(list(...))
}
