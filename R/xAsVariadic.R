
#' xAsVariadic
#'
#' Return a variadic function that passes its arguments
#' as a list to ints underyling function.
#'
#' @details
#'    \code{xAsVariadic} takes a unary function, and
#'    returns a function with ellipsis parametres.
#'    The returned function passes its ellipsis arguments
#'    as a single list to the underlying function.
#'
#' @param
#'    fn a unary function.
#'
#' @return
#'    A variadic function.
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#' @family function_application_functions
#'
#' @example
#'    inst/examples/example-xAsVariadic.R
#'
#' @rdname xAsVariadic
#' @export

xAsVariadic <- function (fn) {
	# (a -> b) -> (... -> b)
	# Return a variadic function that passes its arguments
	# as a list to its underlying function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	fn <- match_fn(fn)

	function (...) {
		"a function returned by xAsVariadic."
		""
		fn(list(...))
	}
}