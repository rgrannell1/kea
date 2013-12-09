
#' xApply
#'
#' Call a function with a list of arguments.
#'
#' @section Uses:
#'    \code{xApply} is an adaptor function that allows variadic functions
#'    to be called with a variable number of arguments at runtime.
#'    For example, if \code{xConcat} is invoked directly it has
#'    to be given a fixed number of lists to concatenate.
#'    If \code{xConcat} is invoked with \code{xApply} then
#'    xConcat can be invoked with any number of lists dynamically.
#'
#' @param
#'    fn an function of any arity.
#'
#' @param coll a list or pairlist. This may be named, but
#'    all names must be parameters of \code{fn}.
#'
#' @return
#'    the return value of \code{fn}.
#'
#' @family
#'    higher_order_functions collection_functions
#'
#' @export

xApply <- function (fn, coll) {
	# function -> [any] -> any
	# call the function f with the list coll.

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	assert(
		!missing(coll), invoking_call,
		exclaim$parameter_missing(coll))

	fn <- dearrowise(fn)
	coll <- dearrowise(coll)

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	assert(
		is_collection(coll), invoking_call,
		exclaim$must_be_collection(coll))

	fn <- match.fun(fn)

	try_higher_order(
		eval(
			as.call(c(fn, coll)),
			envir = parent_frame),
		invoking_call)
}

#' @export

"%$%" <- xApply

#' @export

xApply... <- function (fn, ...) {
	xApply(fn, list(...))
}
