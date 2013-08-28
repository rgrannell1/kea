
#' xFoldl
#' 
#' Fold a function over a collection from left to right with an initial left value.
#'
#' @param fn a binary function that returns a value that 
#'     \code{fn} can later take as its left argument.
#' @param init an arbitrary value.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'     returns \code{init} if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @examples 
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
		ith <- 1
		len_coll <- length(coll)

	    while (ith <= len_coll) {
	    	init <- fn( init, coll[[ind]] )
	    	ith <- ith + 1
	    }
	    init
	}
}

#' @export

xFold <- xFoldl
