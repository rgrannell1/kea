
#' xLift
#'
#' Compose a function with the outputs of other functions.
#'
#' @details
#'    \bold{xLift} takes a function that works on some type of value, and makes that
#'    function work on functions of those values.
#'
#'    \bold{xLift} is a fairly abstract operator, best explained by looking at examples.
#'    The most obvious use for \bold{xLift} is to take normally binary functions like \bold{+},
#'    and \bold{max} that take numbers, and 'lift' it to work on functions of numbers.
#'
#'    A function that multiplies a number can be defined in a function that adds the output of
#'    double that number and triple that number.
#'
#'    \code{double <- x := 2*x}
#'    \code{triple <- x := 3*x}
#'
#'    \code{sextuple <- x := double(x) + triple(x)}
#'    \code{sextuple <- x := '+'(double(x), triple(x))}
#'
#'    This can be viewed as 'adding' the double and triple function, or composing them
#'    with addition.
#'
#'    Similarily, a function to check if a value is a complex number or an integer can be
#'    defined by
#'
#'    \code{is_positive <- x := x > 0}
#'    \code{is_whole <- x := round(x) == x}
#'
#'    \code{is_positive_and_whole <- x := is_positive(x) && is_whole(x)}
#'    \code{is_positive_and_whole <- x := '&&'(is_positive(x), is_whole(x))}
#'
#'    \bold{sextuple} and \bold{is_positive_and_whole} share a common structure. They both
#'    have take one value, and call a binary function (plus, and) with the ouput of two other functions.
#'
#'    \bold{xLift} factors out this pattern, for binary functions and higher arity functions.
#'
#'    \code{sextuple <- xLift_('+', double, triple)}
#'    \code{is_positive_and_whole <- xLift_('&&', is_positive, is_whole)}
#'
#'   Two partially applied forms of xLift are included in arrow, which are useful for cutting down
#'   on the amount of anonymous functions you need to pass to higher-order functions.
#'
#'   \code{xSelect(is_positive \%and\% is_whole, seq(1, 3, by = 0.1))}
#'   \code{list(1, 2, 3)}
#'
#' @param
#'    fn1 a function. The left function to compose with locical and or logical or.
#'
#' @param
#'    fn2 a function. The right function to compose with locical and or logical or.
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

	parent_frame <- parent.frame()

	MACRO( Must $ Not_Be_Missing(fn) )
	MACRO( Must $ Not_Be_Missing(fns) )

	MACRO( Must $ Be_Fn_Matchable(fn) )
	MACRO( Must $ Be_Collection(fns) )

	MACRO( Must $ Be_Collection_Of_Fn_Matchable(fns) )

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

xLift_ <- function (fn, ...) {
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
