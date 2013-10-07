
#' xReduce
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
#' @examples inst/examples/blank.R
#' @export

xReduce <- function (fn, coll) {
	# (any -> any -> any) -> Collection any -> any
	# fold a list, starting from the left.
	
	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))

	assert(
		is_fn_matchable(strs), pcall, 
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)
	
	assert(
		xArity(fn) %in% c(2, Inf), pcall)

	if (length(coll) == 0) {
		coll
	} else if (length(coll) == 1) {
		coll[[1]]
	} else {
		
		init <- coll[[1]]
		coll <- xRest(coll)

		for (ith in seq_along(coll)) {
			init <- fn( init, coll[[ith]] )
		}
		init
	}
}

#' @export

xReducel <- xReduce
