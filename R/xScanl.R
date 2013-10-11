
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
#' @family higher_order_function
#'
#' @export

xScanl <- function (fn, init, coll) {
	# (any -> any -> any) -> any -> Collection any -> [any]
	# scan across list, starting from the right.
	
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
		is_fn_matchable(fn), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	scanned <- c( init, vector("list", length(coll)) )

	if (length(coll) == 0) {
		init
	} else {	
		for (ith in seq_along(coll)) {			
			scanned[[ith + 1]] <- fn( scanned[[ith]], coll[[ith]] )
		}
		scanned
	}
}
