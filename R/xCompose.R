
#' xCompose
#'
#' Take several functions and return a composite function.
#'
#' @details
#'
#' In contrast to many functional languages and frameworks,
#' Arrow's compose function does not require the order of
#' functions to be reversed. The following equation holds
#'
#' \code{(f \%of\% g)(x) == f(g(x))}
#'
#' for Arrow function composition.
#'
#' The classic example of function composition (apart from
#' Church numerals) is composing linear functions.
#'
#' \code{f <- x := 2*x}
#'
#' \code{g <- x := 4*x}
#'
#' \code{h <- x := 1*x}
#'
#' \code{(f \%of\% g \%of\% h)(1)}
#'
#' \code{(x := 2*(4*(1*x)) )(1)}
#'
#' 8
#'
#' In this case the output of one function is piped to
#' another:
#'
#' \code{1 => 1*1 => 1*4 => 4*2 => 8}
#'
#' In this case function composition behaves exactly like
#' multiplication.
#'
#' In more typical use of \bold{xCompose} is to reduce the
#' number of anonymous functions needed by some programs.
#'
#' \code{xMap(sqrt \%of\% sqrt, 1:10)}
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
#'
#' @example
#'    inst/examples/example-xCompose.R
#'
#' @rdname xCompose
#' @export

xCompose <- function (fns) {
	# Collection function -> function

	invoking_call <- sys.call()

	insist$must_be_collection(fns, invoking_call)
	insist$must_be_collection_of_fn_matchable(fns, invoking_call)

	fns <- lapply(fns, match_fn)

	function (...) {
		"a function returned by xCompose."
		""
		invoking_call <- sys.call()

		val <- c(...)

		for ( ith in rev(seq_along(fns)) ) {

			fn <- fns[[ith]]
			val <- try_hof( fn(val), invoking_call )
		}

		val
	}
}

#' @rdname xCompose
#' @export

xCompose... <- function (...) {
	xCompose(list(...))
}

#' @export

'%of%' <- function (fn1, fn2) {
	xCompose(list(fn1, fn2))
}

