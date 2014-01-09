
#' xJuxtapose
#'
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @param
#'    fns a list or pairlist of functions.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    a variadic function.
#'
#' @section Corner Cases:
#'    If no functions are provided the empty list is returned.
#'
#' @family function_modifying_functions
#'
#' @family function_combining_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xJuxtapose
#' @export

xJuxtapose <- function (fns) {
	# Recursive fns -> function

	invoking_call <- sys.call()

	assert(
		is_recursive(fns), invoking_call,
		exclaim$must_be_recursive(fns))

	assert(
		all( vapply(fns, is_fn_matchable, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_matchable(
			fns, summate(fns)) )

	fns <- lapply(fns, match_fn)

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

#' @rdname xJuxtapose
#' @export

xJuxtapose... <- function (...) {
	xJuxtapose(list(...))
}
