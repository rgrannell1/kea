
#' Fold a function over a collection from left to right.
#'
#' @param fn a binary function that returns a value 
#'	 that \code{fn} can later take as its left argument,
#'	 or a string or symbol naming such a function.
#' @param coll a list, pairlist or vector of any length.
#'
#' @return an arbitrary value, depending on the function \code{f}.
#'
#' @section Corner Cases:
#'	 returns \code{coll} if \code{coll} 
#'	 is length-zero or length-one.
#'
#' @export

#| function: xReducel version: 0.1 finished: false 

xReducel <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.
	
	pcall <- sys.call()
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(coll) < 2) {
		coll
	} else {
		ind <- 1
		
		init <- xFirst(coll)
		coll <- xRest(coll)

		len_collection <- length(coll)

		while (ind <= len_collection) {
			init <- fn( init, coll[[ind]] )
			ind <- ind + 1
		}
		init
	}
}

#' @export

xReduce <- xReducel
