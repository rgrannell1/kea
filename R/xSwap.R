
#' xSwap
#' 
#' Return a function that substitutes certain values of its underlying function with new values.
#'
#' @param ... any arbitrary number of \code{list(value, value)}.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xSwap <- function (fn, ...) {
	# function -> Recursive Recursive any -> any
	# wrap a function in another function that substitutes 
	# certain values for other values.

	pcall <- sys.call()

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)
	
	colls <- list(...)

	assert(
		all(sapply(colls, is.recursive)), pcall)

	assert(
		all(sapply(colls, length) == 2), pcall)

	fn <- match.fun(fn)

	if (length(colls) == 0) {
		fn
	} else {
	
		do.call("function", list(
			as.pairlist(formals(fn)),
			bquote({
				out <- .(call_with_params("fn", fn))

				for (pair in colls) {
					if (identical( pair[[1]], out )) {
						return ( pair[[2]] )
					}
				}
				out
			})
		))
	}
}
