
#' xLimit
#'
#' Create a function that can call its underlying
#' function a limited number of times.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @param
#'    num a positive whole number.
#'
#' @return
#'    a function with the same parameters as \code{fn}.
#'
#' @family
#'    higher_order_functions
#'
#' @export

xLimit <- function (fn, num) {
	# integer -> function -> function

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parameter_missing(num))

	assert(
		!missing(fn), invoking_call,
		exclaim$parameter_missing(fn))

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) %in% 0:1, invoking_call,
		exclaim$must_have_length(num, 0:1))

	assert(
		num > 0, invoking_call,
		exclaim$must_be_whole(num))

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(fn))

	fn <- match.fun(fn)

	.count <- 0

	do.call( 'function', list(
		as.pairlist(xFormals(fn)),
		bquote({
			"a function created by xLimit."
			""
			if (.count < num) {
				.count <<- .count + 1
				.( call_with_params('fn', fn) )
			} else {
				Null
			}
	}) ))
}
