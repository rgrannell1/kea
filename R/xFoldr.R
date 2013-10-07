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

xFoldr <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> any
	# fold a list, starting from the right
	
	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(init), pcall, 
		exclaim$parameter_missing(init))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is.vector(coll) || is.pairlist(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(2, Inf), pcall)

	if (length(coll) == 0) {
		init
	} else {
		for (ith in length(coll):1) {
			init <- fn( coll[[ith]], init )
		}
		init
	}
}