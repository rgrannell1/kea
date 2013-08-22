

#' Fold a function over a collection from left to right with an initial value, keeping intermediate values.
#'
#' @param fn a binary function that returns a value that \code{f} can later take as its right argument,
#' or a string or symbol naming such a function.
#' @param initial an arbitrary value.
#' @param collection a list, pairlist or vector of any length.
#'
#' @return a list with its initial element being \code{collection}, and 
#'     containing \code{length(collection) + 1}.
#'
#' @section Corner Cases:
#'     returns \code{list(initial)} if \code{collection} is length-zero.
#'
#' @export

#| function: xScanl version: 0.1 finished: false 

xScanl <- function (fn, initial, collection) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.
	
	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a('any', initial, pcall)
	require_a("collection", collection, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	scanned <- c( initial, vector("list", length(collection)) )

	if (length(collection) == 0) {
		initial
	} else {
	
		ith <- 1
		while (ith <= length(collection)) {
			
			scanned[[ith + 1]] <- fn(
				scanned[[ith]],
				collection[[ith]]
			)
			ith <- ith + 1
		}
		scanned
	}
}
