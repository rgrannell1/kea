
#' xDelay
#'
#' Delay the evalution of a function after invokation
#' for a set amount of time.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @return
#'    a function with the same parametres as \code{fn}.
#'
#' @section Corner Cases:
#'    if \code{num} is zero then \code{fn} is returned untouched.
#'
#' @family function_modifying_functions
#'
#' @family time_functions
#'
#' @rdname xDelay
#' @export

xDelay <- function (fn, num) {
	# function -> number -> function

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	assert(
		is.numeric(num) && num >= 0, invoking_call,
		exclaim$must_be_greater_than(
			num, 0, summate(num)) )

	fn <- match_fn(fn)
	remove(invoking_call)

	if (num == 0) {
		fn
	} else {

		do.call("function", list(
			as.pairlist(xFormalsOf(fn)),
			bquote({
				Sys.sleep(num)
				.( call_with_params("fn", fn) )
			})
		))
	}
}
