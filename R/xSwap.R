
#' xSwap
#' 
#' Return a function that substitutes certain values of an underlying function.
#'
#' @param coll a collection of two element lists, value, value
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll is length-zero}.
#' @template glossary
#'
#' @examples 
#' @export

xSwap <- function (fn, coll) {

	pcall <- sys.call()
	require_a('functionable', fn, pcall)
	require_a(c(
		'recursive_of_length_two', 
		'recursive_of_recursive'), coll, pcall)

	fn <- match.fun(fn)

	if (length(coll) == 0) {
		fn
	} else {
	
		do.call("function", list(
			as.pairlist(formals(fn)),
			bquote({

				out <- .(call_with_params("fn", fn))

				for (pair in coll) {
					if (identical( pair[[1]], out )) {
						return ( pair[[2]] )
					}
				}

				out
			})
		))		
	
	}
}
