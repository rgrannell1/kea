
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

	invoking_call <- sys.call()

	fns <- lapply(fns, dearrowise)

	assert(
		is_recursive(fns), invoking_call,
		exclaim$must_be_recursive(fns))

	assert(
		all(sapply(fns, is_fn_matchable)), invoking_call,
		exclaim$must_be_recursive_of_matchable("fns"))

	fns <- lapply(fns, match.fun)

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
