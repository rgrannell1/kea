
#' xThread
#'
#' Iteratively apply a value to list of functions.
#'
#' @param init an arbitrary value
#' @param ... several unary functions.
#'
#' @return a list.
#'
#' @section Corner Cases: q
#'     returns the empty list if \code{coll} is length-zero.
#' @template glossary
#'
#' @example inst/examples/blank.R
#' @export

xThread <- function (init, ...) {
	# any -> .... -> any
	# iteratively apply a value to each function in a list.

	parent_call <- sys.call()

	assert(
		!missing(init), parent_call,
		exclaim$parameter_missing(init))

	init <- dearrowise(init)
	fns <- lapply(list(...), dearrowise)

	assert(
		all(sapply(fns, is_fn_matchable)), parent_call,
		exclaim$must_be_recursive_of_matchable(fns))

	for (ith in seq_along(fns)) {

		init <- fns[[ith]]( init )
	}
	init
}
