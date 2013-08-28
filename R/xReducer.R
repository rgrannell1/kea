
#' Fold a function over a collection from right to left.
#'
#' @param fn a binary function that returns a value that 
#'	 \code{fn} can later take as its right argument,
#'	 or a string or symbol naming such a function.
#' @param coll a list, pairlist or vector of any length.
#'
#' @return an arbitrary value, depending on the function \code{f}.
#'
#' @section Corner Cases:
#'	 returns \code{coll} if \code{coll} is length-zero or length-one.
#'
#' @family arrow-folds
#' @export

#| function: xReducer version: 0.1 finished: false 

xReducer <- function (fn, coll) {
	# (a -> b -> a) -> [b] -> a
	# fold a list, starting from the left.
	
	pcall <- sys.call()	
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(coll) < 2) {
		coll
	} else {
		ind <- length(coll) - 1
		
		init <- xFirst(coll)
		coll <- xRest(coll)

		while (ind > 0) {
			init <- fn( coll[[ind]], init )
			ind <- ind - 1
		}
		init
	}
}