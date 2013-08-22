
#' Fold a function over a collection from right to left.
#'
#' @param fn a binary function that returns a value that 
#'     \code{fn} can later take as its right argument,
#'     or a string or symbol naming such a function.
#' @param collection a list, pairlist or vector of any length.
#'
#' @return an arbitrary value, depending on the function \code{f}.
#'
#' @section Corner Cases:
#'     returns \code{collection} if \code{collection} is length-zero or length-one.
#'
#' @family arrow-folds
#' @export

#| function: xReducer version: 0.1 finished: false 

xReducer <- function (fn, collection) {
	# (a -> b -> a) -> [b] -> a
	# fold a list, starting from the left.
	
	pcall <- sys.call()	
	require_a("functionable", fn, pcall)
	require_a("collection", collection, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(collection) < 2) {
		collection
	} else {
		ind <- length(collection) - 1
		
		initial <- xFirst(collection)
		collection <- xRest(collection)

	    while (ind > 0) {
	    	initial <- fn( collection[[ind]], initial )
	    	ind <- ind - 1
	    }
	    initial
	}
}