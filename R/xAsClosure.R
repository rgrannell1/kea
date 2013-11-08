
#' xAsClosure
#'
#' Convert a primitive function to a closure.
#'
#' @param fn an arbitrary function.
#'
#' @return a function (closure).
#'
#'
#'
#' @section Corner Cases:
#'	 xAsClosure does not work for every primitive function (for example 'c'),
#'	 so caution should be taken when using this function.
#'
#' @example inst/examples/blank.R
#' @export

xAsClosure <- function (fn) {
	# (a -> b) -> (a -> b)
	# convert a primitive function to a closure.

	parent_call <- sys.call()

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	fn <- match_fn(fn)

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
