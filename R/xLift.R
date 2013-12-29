
#' xLift
#'
#' Compose a binary function with two other functions.
#'
#' @param
#'    fn a binary function.
#'
#' @param
#'    fns a list or pairlist of binary functions.
#'
#' @return
#'    returns a unary function of x.
#'
#' @family higher_order_functions
#'
#' @family function_modifying_functions
#'
#' @family function_combining_functions
#'
#' @family variadic_functions
#'
#' @export

xLift <- function (fn, fns) {
	# (any -> any -> any) -> [(... -> any)] -> any
	# the phoenix or Phi combinator

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(fns), invoking_call,
		exclaim$parametre_missing(fns))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, profile_object(fn)) )

	assert(
		is_collection(fns), invoking_call,
		exclaim$must_be_collection(
			fns, profile_object(fns)) )

	assert(
		all(sapply(fns, is_fn_matchable)), invoking_call,
		exclaim$must_be_recursive_of_matchable(
			fns, profile_object(fns)) )

	fn <- match.fun(fn)
	fns <- lapply(fns, match.fun)

	function (...) {
		"A function created by xLift."
		""
		do.call(fn,
			lapply(fns, function (lifted) {
				lifted(...)
			}) )
	}
}

#' @export

xLift... <- function (fn, ...) {
	do.call( xLift, list(fn, list(...)) )
}
