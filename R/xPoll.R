
#' xPoll
#' 
#' Count the number of times a function returns true when mapped over a collection.
#'
#' @param pred a unary predicate function.
#' @param coll a collection.
#'
#' @return a non-negative whole number.
#'
#' @template glossary
#'
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
#' @export

xPoll <- function (pred, coll) {
	# (any -> logical) -> Collection any -> integer
	# return the number of elements for which a predicate is true.

	pcall <- sys.call()
	
	assert(
		!missing(pred), pcall,
		exclaim$parameter_missing(pred))
	assert(
		!missing(coll), pcall, 
		exclaim$parameter_missing(coll))
	
	assert(
		is_fn_matchable(pred), pcall,
		exclaim$must_be_matchable(pred))

	assert(
		is_collection(coll), pcall,
		exclaim$must_be_collection(coll))

	pred <- match.fun(pred)
	
	if (length(coll) == 0) {
		0
	} else {
		count <- 0
		for (ith in seq_along(coll)) {
			
			is_match <- pred( coll[[ith]] )
			
			assert(is.logical(is_match), pcall)

			if (isTRUE(is_match)) {
				count <- count + 1
			}
		}
		count
	}
}
