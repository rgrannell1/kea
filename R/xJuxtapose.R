
#' xJuxtapose
#'
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @param
#'    fns a list or pairlist of functions.
#'
#' @return
#'    a variadic function.
#'
#' @section Corner Cases:
#'    If no functions are provided the empty list is returned.
#'
#' @family higher_order_functions
#'
#' @export

xJuxtapose <- function (fns) {
	# Recursive fns -> function

	invoking_call <- sys.call()

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
			"a function created by xJuxtapose."

			invoking_call <- sys.call()

			try_higher_order(
				lapply(fns, function (fn) fn(...)),
				invoking_call)
		}
	}
}

#' @export

xJuxtapose... <- function (...) {
	xJuxtapose(list(...))
}
