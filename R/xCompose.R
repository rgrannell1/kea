
#' xCompose
#'
#' Take several functions and return a composite function.
#'
#' @section Type Signature:
#'     |function| -> (..any -> any)
#'
#' @details
#'
#'    Kiwi's function composition is largely done with the infix
#'    operators \%of\% and \%then\%.
#'
#'    \bold{\%then\%} is the classic function composition operator;
#'
#'    \code{(as.numeric \%then\% sqrt \%then\% print)('10')}
#'
#'    Like methods, the order of function execution is from left to right;
#'    convert a string to a number then take the square root then print the value.
#'    The reversed form \%of\% is equally as common.
#'
#'    The classic example of function composition (apart from
#'    Church numerals) is composing linear functions.
#'
#'    \code{f <- x := 2*x}
#'
#'    \code{g <- x := 4*x}
#'
#'    \code{h <- x := 1*x}
#'
#'    \code{(f \%of\% g \%of\% h)(1)}
#'
#'    \code{(x := 2*(4*(1*x)) )(1)}
#'
#'    xCompose
#'
#'    In this case the output of one function is piped to
#'    another:
#'
#'    \code{1 => 1*1 => 1*4 => 4*2 => 8}
#'
#'    In this case function composition behaves exactly like
#'    multiplication.
#'
#'    In more typical use of \bold{xCompose} is to reduce the
#'    number of anonymous functions needed by some programs.
#'
#'    \code{xMap(sqrt \%of\% sqrt, 1:10)}
#'
#' @param
#'    fn1 a function.
#'
#' @param
#'    fn2 a function.
#'
#' @param
#'    fns a collection of functions. The functions to
#'    compose with each other.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A variadic function.
#'
#' @family function_modifying_functions
#'
#' @family function_combining_functions
#'
#' @template
#'    Variadic
#'
#' @section Corner Cases:
#'    \bold{xCompose} throws an error if no functions are passed to it, as it has
#'    no sensible definition in this case.
#'
#' @example
#'    inst/examples/example-xCompose.R
#'
#' @rdname xCompose
#' @export

xCompose <- MakeFun(function (fns) {

	MACRO( Must $ Be_Longer_Than(0, fns) )

	function (...) {
		"a function returned by xCompose."
		""

		# -- can't use do.call here, since the value has
		# -- to be passed to several functions.
		val <- c(...)

		for ( ith in rev(seq_along(fns)) ) {

			fn <- fns[[ith]]
			val <- fn(val)
		}

		val
	}
})

#' @rdname xCompose
#' @export

xCompose_ <- MakeVariadic(xCompose, 'fns')

#' @rdname xCompose
#' @export

'%of%' <- function (fn1, fn2) {
	xCompose(list(fn1, fn2))
}

#' @rdname xCompose
#' @export

'%then%' <- function (fn1, fn2) {
	xCompose(list(fn2, fn1))
}
