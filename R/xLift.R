
#' xLift
#'
#' Compose a function with two other functions.
#'
#' @details
#'    \bold{xLift} takes a function that works on some type of value, and makes that
#'    function work on functions of those values.
#'
#'
#'    Two partially applied version of \bold{xLift} exist; \%and\% & \%or\%.
#'    These are primarily intended for use with higher-order functions that take a predicate,
#'    allowing several predicates to be stringed together into a new predicate.
#'
#' \code{xSelect(is.integer %or% is.complex, list(1L, 1+1i, 2L, 'string'))}
#'
#' \code{list(1L, 1+1i, 2L, 'string')}
#'
#' @param
#'    fn1 a function.
#'
#' @param
#'    fn2 a function.
#'
#' @param
#'    fn a binary function.
#'
#' @param
#'    fns a collection functions.
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

xLift <- MakeFun(function (fn, fns) {
	# (any -> any -> any) -> [(... -> any)] -> any
	# the phoenix or Phi combinator

	invoking_call <- sys.call()
	parent_frame <- parent.frame()

	MACRO( arrow ::: Must $ Not_Be_Missing(fn) )
	MACRO( arrow ::: Must $ Not_Be_Missing(fns) )

	MACRO( arrow ::: Must $ Be_Fn_Matchable(fn) )
	MACRO( arrow ::: Must $ Be_Collection(fns) )

	MACRO( arrow ::: Must $ Be_Collection_Of_Fn_Matchable(fns) )

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
})

#' @rdname xLift
#' @export

xLift... <- function (fn, ...) {
	do.call( xLift, list(fn, list(...)) )
}

#' @rdname xLift
#' @export

'%or%' <- function (fn1, fn2) {
	xLift('||', list(fn1, fn2))
}

#' @rdname xLift
#' @export
'%and%' <- function (fn1, fn2) {
	xLift('&&', list(fn1, fn2))
}
