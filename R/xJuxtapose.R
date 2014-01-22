
#' xJuxtapose
#'
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @details
#'    \code{xJuxtapose} is a method of applying one value to several functions
#'    simultaneously. The function returned by \code{xJuxtapose} returns
#'    a list - on element for each input function to \code{xJuxtapose}.
#'    Each element of this list contains the result of calling an element of
#'    \code{fns} with the arguments supplied to \code{xJuxtapose}'s return function.
#'
#' @param
#'    fns a list or pairlist of functions.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A variadic function.
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

	insist$must_be_collection_of_fn_matchable(fns, invoking_call)

	fns <- lapply(fns, match_fn)

	if (length(fns) == 0) {
		list()
	} else {

		function (...) {
			"a function created by xJuxtapose."
			""
			invoking_call <- sys.call()

			try_hof(
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
