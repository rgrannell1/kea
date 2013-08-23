#'
#' Fold a function over a collection from left to right, with an init value.
#'
#' @param fn a binary function that returns a value that 
#'     \code{fn} can later take as its right argument,
#'     or a string or symbol naming such a function.
#' @param init an arbitrary value.
#' @param coll a list, pairlist or vector of any length.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'     returns \code{init} if \code{coll} is length-zero.
#'
#' @export

#| function: xFoldl version: 0.1 finished: false 

xFoldl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the left"
	
	pcall <- sys.call()	
	require_a("functionable", fn, pcall)
	require_a('any', init, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(coll) == 0) {
		init
	} else {
		ind <- 1
		len_coll <- length(coll)

	    while (ind <= len_coll) {
	    	init <- fn( init, coll[[ind]] )
	    	ind <- ind + 1
	    }
	    init
	}
}

#' @export

xFold <- xFoldl
