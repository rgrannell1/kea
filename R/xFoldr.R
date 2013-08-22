#'
#' Fold a function over a collection from right to length, with an init value.
#'
#' @param fn a binary function that returns a value that \code{fn} can later take as its right argument,
#' or a string or symbol naming such a function.
#' @param init an arbitrary value.
#' @param coll a list, pairlist or vector of any length.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'     returns \code{init} if \code{coll} is length-zero.
#'
#' @export

#| function: xFoldr version: 0.1 finished: false 

xFoldr <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right
	
	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a('any', init, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(coll) == 0) {
		init
	} else {
		ind <- length(coll)

	    while (ind > 0) {
	    	init <- fn( coll[[ind]], init )
	    	ind <- ind - 1
	    }
	    init
	}
}