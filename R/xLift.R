
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
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a unary function of x.
#'
#' @family function_modifying_functions
#'
#' @family function_combining_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xLift
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

	assert_is_fn_matchable(fn, invoking_call)

	assert(
		is_collection(fns), invoking_call,
		exclaim$must_be_collection(
			fns, summate(fns)) )

	assert(
		all( vapply(fns, is_fn_matchable, logical(1)) ), invoking_call,
		exclaim$must_be_recursive_of_matchable(
			fns, summate(fns)) )

	fn <- match_fn(fn)
	fns <- lapply(fns, match_fn)

	function (...) {
		"A function created by xLift."
		""
		do.call(fn,
			lapply(fns, function (lifted) {
				lifted(...)
			}) )
	}
}

#' @rdname xLift
#' @export

xLift... <- function (fn, ...) {
	do.call( xLift, list(fn, list(...)) )
}
