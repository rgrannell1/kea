
#' xAsClosure
#'
#' Convert a primitive function to a closure.
#'
#' @details
#'    xAsClosure attempts to wrap a primitive function in a closure
#'    with the same arguments as a the primitive function, before
#'    passing the arguments to that primitive function.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @return
#'    A function (closure).
#'
#' @section Corner Cases:
#'    \code{xAsClosure} does not work for every primitive function (for example 'c'),
#'    so caution should be taken when using this function.
#'
#' @family function_modifying_functions
#'
#' @example
#'    inst/examples/example-xAsClosure.R
#'
#' @rdname xAsClosure
#' @export

xAsClosure <- function (fn) {
	# (a -> b) -> (a -> b)
	# convert a primitive function to a closure.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	insist$must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)

	if (is.primitive(fn)) {

		do.call("function", list(
			as.pairlist(xFormalsOf(fn)),
			bquote({
				"A function created by xAsClosure."
				""
				.(call_with_params("fn", fn))
			})
		))

	} else {
		fn
	}
}
