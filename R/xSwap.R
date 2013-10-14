
#' xSwap
#' 
#' Return a function that substitutes certain values of its underlying function with new values.
#'
#' @param fn a function.
#' @param ... any arbitrary number of \code{list(value, value)}.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
#' @export

xSwap <- function (fn, ...) {
	# function -> Recursive Recursive any -> any
	# wrap a function in another function that substitutes 
	# certain values for other values.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))
	
	colls <- list(...)

	assert(
		all(sapply(colls, is.recursive)), pcall,
		exclaim$must_be_recursive_of_collections(colls))

	assert(
		all(sapply(colls, length) == 2), pcall,
		exclaim$must_be_collection_of_length(colls))

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
