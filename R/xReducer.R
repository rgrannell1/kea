
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

xReducer <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.
	
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
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {
		
		init <- coll[[ length(coll) ]]
		coll <- xInit(coll)

		for (ith in (length(coll) - 1):1) {
			init <- fn( coll[[ith]], init )
		}
		init
	}
}