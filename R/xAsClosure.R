
#' xAsClosure
#' 
#' Convert a primitive function to a closure.
#'
#' @param fn an arbitrary function.
#'
#' @return a function (closure).
#'
#' @template glossary
#'
#' @section Corner Cases: 
#'	 xAsClosure does not work for every primitive function (for example 'c'),
#'	 so caution should be taken when using this function.
#'
#' @examples inst/examples/blank.R
#' @export

xAsClosure <- function (fn) {
	# (a -> b) -> (a -> b)
	# convert a primitive function to a closure.

	pcall <- sys.call()
	
	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	
	assert(
		is_fn_matchable(fn), pcall, 
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
