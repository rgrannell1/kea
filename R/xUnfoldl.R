
#' xUnfold
#' 
#' Generate a list of values from an initial value.
#'
#' @param pred a unary function.
#' @param fn a unary function that returns a length-two collection.
#' @param init an arbitrary value.
#'
#' @return a list.
#'
#' @section Corner Cases: 
#'     returns the empty list if \code{init} is length-zero.
#' @template glossary
#'
#' @examples inst/examples/blank.R
#' @export

xUnfold <- function (pred, fn, init) {
	# (any -> boolean) -> (any -> [any, any]) -> any -> [any]
	# generate a list of values from an initial value.

	pcall <- sys.call()

	assert(
		!missing(pred), pcall)
	assert(
		!missing(fn), pcall)
	assert(
		!missing(init), pcall)

	assert(
		is.function(pred) || is.symbol(pred) || 
		(is.character(pred) && length(pred) == 1), pcall)

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall)

	fn <- match.fun(fn)
	pred <- match.fun(pred)
	
	assert(
		xArity(fn) %in% c(1, Inf), pcall)
	assert(
		xArity(pred) %in% c(1, Inf), pcall)

	acc <- list(init)

	while (pred( acc[[ length(acc) ]]  )) {
		
		fn_out <- fn( acc[[ length(acc) ]] )

		assert(
			length(fn_out) == 2, pcall)

		acc[length(acc)] <- fn_out[[1]]
		acc[length(acc) + 1] <- fn_out[[2]]
	}
	acc
	
}

#' @export 

xUnfoldl <- xUnfold


