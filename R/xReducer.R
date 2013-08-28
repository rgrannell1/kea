
#' xReducer
#' 
#' Fold a function over a collection from right to left.
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

#| function: xReducer version: 0.1 finished: false 

xReducer <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.
	
	pcall <- sys.call()	
	require_a("functionable", fn, pcall)
	require_a("collection", coll, pcall)

	fn <- match.fun(fn)
	require_a('binary function', fn, pcall)

	if (length(coll) == 0) {
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {
		ind <- length(coll) - 1
		
		init <- coll[[1]]
		coll <- xRest(coll)

		while (ind > 0) {
			init <- fn( coll[[ind]], init )
			ind <- ind - 1
		}
		init
	}
}