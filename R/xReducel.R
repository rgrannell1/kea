
#' xReducel
#' 
#' Fold a function over a collection from left to right.
#'
#' @param fn a binary function that returns a value 
#'	 that \code{fn} can later take as its left argument.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{coll} is length-zero, and returns the 
#'     value inside \code{coll} if coll is length-one.
#' @template glossary
#'
#' @examples 
#' @export

xReducel <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.
	
	pcall <- sys.call()
	require_a(traits$functionable, fn, pcall)
	require_a(traits$collection, coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(coll) == 0) {
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {
		ith <- 1
		
		init <- coll[[1]]
		coll <- xRest(coll)

		len_collection <- length(coll)

		while (ith <= len_collection) {
			init <- fn( init, coll[[ith]] )
			ith <- ith + 1
		}
		init
	}
}

#' @export

xReduce <- xReducel
