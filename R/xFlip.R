
#' xFlip
#'
#' Reverse the formal parametres of a function.
#'
#' @param fn an arbitrary function.
#'
#' @return a function of the same arity as \code{fn}.
#'
#'
#'
#' @family higher_order_functions
#'

#' @export

xFlip <- function (fn) {
	# function -> function
	#' reverse the parameters of a function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	fn <- dearrowise(fn)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)
	remove(invoking_call)

	do.call('function', list(
		as.pairlist(rev( xFormals(fn) )),
		body(fn)
	))
}

#' @export

xCardinal <- xFlip

#' @export

xC <- xFlip
