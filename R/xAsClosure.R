
#' xAsClosure
#'
#' Convert a primitive function to a closure.
#'
#' @details
#'    \bold{xAsClosure} takes a primitive function and wraps it in a
#'    normal R function that passes its arguments to the underlying
#'    primitive function.
#'
#'    The most commonly encountered R primitive functions are the arithmetic
#'    operators. \bold{xAsClosure} can convert these to normal functions.
#'
#'    \code{xAsClosure('+')}
#'
#'    \code{function (e1, e2) fn(e1, e2)}
#'
#'    doc
#'
#' @param
#'    fn an arbitrary function. Either a primitive or
#'    non-primitive function to convert to a non-primitive function.
#'
#' @return
#'    A non-primitive closure.
#'
#' @section Corner Cases:
#'    \code{xAsClosure} does not work for every primitive function
#'    (for example \bold{c( )}), so caution should be taken when
#'    using this function.
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

	insist $ must_be_fn_matchable(fn, invoking_call)

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
