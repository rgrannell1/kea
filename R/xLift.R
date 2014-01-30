
#' xLift
#'
#' Compose a binary function with two other functions.
#'
#' @details
#'
#'
#' Two partially applied version of \bold{xLift} exist;
#' \bold{\%or\%} and \bold{\%and\%}. These exist to solve
#'
#' \code{is.na(x) || is.null(x) || is.nan(x)}
#'
#'
#' \code{(is.na \%or\% is.null \%or\% is.nan)(x)}
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
#' @example
#'    inst/examples/example-xLift.R
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

	insist $ must_be_fn_matchable(fn, invoking_call)

	insist $ must_be_collection(fns, invoking_call)
	insist $ must_be_collection_of_fn_matchable(fns, invoking_call)

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

