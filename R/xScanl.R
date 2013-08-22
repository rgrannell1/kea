

#' Fold a function over a collection from left to right with an init value, keeping intermediate values.
#'
#' @param fn a binary function that returns a value that \code{f} can later take as its right argument,
#' or a string or symbol naming such a function.
#' @param init an arbitrary value.
#' @param coll a list, pairlist or vector of any length.
#'
#' @return a list with its init element being \code{coll}, and 
#'     containing \code{length(coll) + 1}.
#'
#' @section Corner Cases:
#'     returns \code{list(init)} if \code{coll} is length-zero.
#'
#' @export

#| function: xScanl version: 0.1 finished: false 

xScanl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.
	
	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a('any', init, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	scanned <- c( init, vector("list", length(coll)) )

	if (length(coll) == 0) {
		init
	} else {
	
		ith <- 1
		while (ith <= length(coll)) {
			
			scanned[[ith + 1]] <- fn(
				scanned[[ith]],
				coll[[ith]]
			)
			ith <- ith + 1
		}
		scanned
	}
}
