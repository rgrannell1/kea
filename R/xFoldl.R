
#' xFoldl
#' 
#' Fold a function over a collection from left to right with an initial left value.
#'
#' @param fn a binary function that returns a value that 
#'	 \code{fn} can later take as its left argument.
#' @param init an arbitrary value.
#' @param coll a collection.
#'
#' @return an arbitrary value, depending on the function \code{fn}.
#'
#' @section Corner Cases:
#'	 returns \code{init} if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @examples 
#' @export

xFoldl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the left"
	
	pcall <- sys.call()	

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)
	
	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(2, Inf), pcall)

	if (length(coll) == 0) {
		init
	} else {
		for (ith in length(coll)) {
			init <- fn( init, coll[[ith]] )			
		}
		init
	}
}

#' @export

xFold <- xFoldl
