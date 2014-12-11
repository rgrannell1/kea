
#' xLift
#'
#' Compose a function with the outputs of other functions.
#'
#' @section Type Signature:
#'     (any -> any -> any) -> |(any -> any)| -> (...any -> any)
#'
#' @details
#'    \bold{xLift} takes a function that works on some type of value, and makes that
#'    function work on functions of those values.
#'
#'    \bold{xLift} is a fairly abstract operator and is best explained by looking at examples.
#'    The most obvious use for \bold{xLift} is to take normally binary functions like \bold{+},
#'    and \bold{max} that take numbers, and 'lift' it to work on functions of numbers.
#'
#'    Here the numeric functions "double" and "triple" are used to define a new function 
#'    "sextuple". Addition usually takes two numbers, but when lifted it adds the result of 
#'    numeric functions.
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
#'    \code{sextuple <- xLift_(`+`, double, triple)}
#''    
#'    Similarily, a function to check if a value is positive and a whole number can be
#'    defined by
#'
#'    \code{is_positive <- x := x > 0}
#'    \code{is_whole <- x := round(x) == x}
#'
#'    \code{is_positive_and_whole <- x := is_positive(x) && is_whole(x)}
#'    \code{is_positive_and_whole <- x := '&&'(is_positive(x), is_whole(x))}
#'
#'    \bold{sextuple} and \bold{is_positive_and_whole} share a common structure. They both
#'    have take one value, and call a binary function (plus, and) with the output of two other functions.
#'
#'    \bold{xLift} factors out this pattern, for binary functions and higher arity functions.
#'
#'    \code{sextuple <- xLift_('+', double, triple)}
#'    \code{is_positive_and_whole <- xLift_('&&', is_positive, is_whole)}
#'
#'   Two partially applied forms of xLift are included in kea; 'and' and 'or', which lifts two functions with
#'   a boolean operator.
#'
#'   \code{xSelect(is_positive \%and\% is_whole, seq(1, 3, by = 0.1))}
#'   \code{list(1, 2, 3)}
#'
#' @param
#'    fn1 a function. The left function to compose with logical and or logical or.
#'
#' @param
#'    fn2 a function. The right function to compose with logical and or logical or.
#'
#' @param
#'    fn a binary function. The function to lift.
#'
#' @param
#'    fns a collection of functions. The functions to transform each argument with before 
#'    passing the result to \bold{fn}
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a unary function of x.
#'
#' @section Corner Cases:
#'    Calls \bold{fn} with no arguments when \bold{fns} is empty.
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

	parent_frame <- parent.frame()

	function (...) {
		"A function created by xLift."
		""
		do.call(fn,
			lapply(fns, function (lifted) lifted(...)) )
	}
})

#' @rdname xLift
#' @export

xLift_ <- MakeVariadic(xLift, 'fns')

#' @rdname xLift
#' @export

'%or%' <- function (fn1, fn2)
	function (...)
		fn1(...) || fn2(...)



#' @rdname xLift
#' @export

'%and%' <- function (fn1, fn2)
	function (...)
		fn1(...) && fn2(...)


