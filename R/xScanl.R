
#' xScanl
#'
#' Fold a function over a collection from left to right with an initital left value, keeping intermediate values.
#'
#' @param fn a binary function that returns a value that \code{fn} can later take as its right argument.
#' @param init an arbitrary value.
#' @param coll a collection.
#'
#' @return a list with its init element being \code{coll}, and 
#'	 containing \code{length(coll) + 1}.
#'
#' @section Corner Cases:
#'	 returns \code{list(init)} if \code{coll} is length-zero.
#'
#' @template glossary
#'
#' @export

xScanl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.
	
	pcall <- sys.call()

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(2, Inf), pcall)

	scanned <- c( init, vector("list", length(coll)) )

	if (length(coll) == 0) {
		init
	} else {	
		ith <- 1
		while (ith <= length(coll)) {
			
			scanned[[ith + 1]] <- fn( scanned[[ith]], coll[[ith]] )
			ith <- ith + 1
		}
		scanned
	}
}
