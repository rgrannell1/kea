
#' xAsClosure
#'
#' Convert a primitive function to a closure.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @return
#'    a function (closure).
#'
#' @section Corner Cases:
#'    xAsClosure does not work for every primitive function (for example 'c'),
#'    so caution should be taken when using this function.
#'
#' @family higher_order_functions
#'
#' @family function_modifying_functions
#'
#' @export

xAsClosure <- function (fn) {
	# (a -> b) -> (a -> b)
	# convert a primitive function to a closure.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	if (is.primitive(fn)) {

		do.call("function", list(
			as.pairlist(xFormals(fn)),
			bquote({
				.(call_with_params("fn", fn))
			})
		))

	} else {
		fn
	}
}
