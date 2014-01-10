
#' xAsUnary
#'
#' Convert any function to a unary function.
#'
#' @details
#'    xAsUnary takes a function and returns a function that
#'    has one argument. The first element of the argument is
#'    passed to the first parametre of the underlying function,
#'    the second element to the second parametre, and so on.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @return
#'    a unary function of x.
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#' @family function_application_functions
#'
#' @example
#'    inst/examples/example-xAsUnary.R
#'
#' @rdname xAsUnary
#' @export

xAsUnary <- function (fn) {
	# (... -> b) -> (a -> b)
	# dual to xAsVariadic.
	# takes a function that takes a many values and
	# makes it into a function that takes one list.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	fn <- match_fn(fn)
	remove(invoking_call)

	function (x) {
		"a function returned by xAsUnary."
		""
		xApply(fn, x)
	}
}
