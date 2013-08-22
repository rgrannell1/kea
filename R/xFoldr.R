#'
#' Fold a function over a collection from right to length, with an initial value.
#'
#' @param fn a binary function that returns a value that \code{fn} can later take as its right argument,
#' or a string or symbol naming such a function.
#' @param initial an arbitrary value.
#' @param collection a list, pairlist or vector of any length.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'     returns \code{initial} if \code{collection} is length-zero.
#'
#' @export

#| function: xFoldr version: 0.1 finished: false 

xFoldr <- function (fn, initial, collection) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right
	
	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a('any', initial, pcall)
	require_a("collection", collection, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(collection) == 0) {
		initial
	} else {
		ind <- length(collection)

	    while (ind > 0) {
	    	initial <- fn( collection[[ind]], initial )
	    	ind <- ind - 1
	    }
	    initial
	}
}