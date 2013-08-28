#'
#' Fold a function over a collection from right to length, with an init value.
#'
#' @param fn a binary function that returns a value that 
#'	 \code{fn} can later take as its right argument
#' @param init an arbitrary value.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'	 returns \code{init} if \code{coll} is length-zero.
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
		ith <- length(coll)

		while (ith > 0) {
			init <- fn( coll[[ith]], init )
			ith <- ith - 1
		}
		init
	}
}