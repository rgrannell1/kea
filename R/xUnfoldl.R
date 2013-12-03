
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

	invoking_call <- sys.call()

	assert(
		!missing(pred), invoking_call,
		exclaim$parameter_missing(pred))

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(init), invoking_call,
		exclaim$parameter_missing(init))

	pred <- dearrowise(pred)
	fn <- dearrowise(fn)
	init <- dearrowise(init)

	assert(
		is_fn_matchable(pred), invoking_call,
		exclaim$must_be_matchable(pred))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	pred <- match.fun(pred)

	acc <- list(init)

	while (pred( acc[[ length(acc) ]]  )) {

		fn_out <- try_higher_order(
			fn( acc[[ length(acc) ]] ),
			invoking_call)

		acc[length(acc)] <- fn_out[[1]]
		acc[length(acc) + 1] <- fn_out[[2]]
	}
	acc

}

#' @export

xUnfoldl <- xUnfold


