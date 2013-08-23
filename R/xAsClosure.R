
#' Convert a primitive function to a closure
#'
#' @param fn an arbitrary function, or a 
#'     symbol or string identifying such a function.
#' @return a closure with the same parameters as \code{fn}.
#'
#' @export
#'

#| function: xAsClosure version: 0.1 finished: false 

xAsClosure <- function (fn) {
	# (a -> b) -> (a -> b)
	# convert a primitive function to a closure.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)

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
