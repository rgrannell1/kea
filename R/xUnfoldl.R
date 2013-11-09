
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

#'
#' @family higher_order_functions
#'
#' @example inst/examples/blank.R
#' @export

xUnfold <- function (pred, fn, init) {
	# (any -> boolean) -> (any -> [any, any]) -> any -> [any]
	# generate a list of values from an initial value.

	parent_call <- sys.call()

	assert(
		!missing(pred), parent_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(fn), parent_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), parent_call,
		exclaim$parameter_missing(init))

	pred <- dearrowise(pred)
	fn <- dearrowise(fn)
	init <- dearrowise(init)

	assert(
		is_fn_matchable(pred), parent_call,
		exclaim$must_be_matchable(pred))

	assert(
		is_fn_matchable(fn), parent_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	pred <- match.fun(pred)

	acc <- list(init)

	while (pred( acc[[ length(acc) ]]  )) {

		fn_out <- fn( acc[[ length(acc) ]] )

		acc[length(acc)] <- fn_out[[1]]
		acc[length(acc) + 1] <- fn_out[[2]]
	}
	acc

}

#' @export

xUnfoldl <- xUnfold


