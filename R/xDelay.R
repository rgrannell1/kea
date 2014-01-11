
#' xDelay
#'
#' Delay the evalution of a function after invokation
#' for a set amount of time.
#'
#' @details
#'    \code{xDelay} sleeps for a preset amount of time
#'    \bold{before} executing its underlying function.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @param
#'    num a nonnegative whole number.
#'
#' @return
#'    A function with the same parametres as \code{fn}.
#'
#' @section Corner Cases:
#'    If \code{num} is zero then \code{fn} is returned untouched.
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
				"A function created by xDelay."
				""
				Sys.sleep(num)
				.( call_with_params("fn", fn) )
			})
		))
	}
}
