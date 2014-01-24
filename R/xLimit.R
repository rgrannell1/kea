
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
#'    A function with the same parametres as \bold{fn}.
#'
#' @family time_functions
#'
#' @rdname xLimit
#' @export

xLimit <- function (fn, num) {
	# integer -> function -> function
	# limit how many times a function can be called.

	invoking_call <- sys.call()

	assert(
		!missing(num), invoking_call,
		exclaim$parametre_missing(num))

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	insist$must_be_collection(num, invoking_call)

	num <- to_value_unit(as_typed_vector(num, 'numeric'))

	insist$must_be_of_length(num, 1)
	insist$must_be_whole(num, invoking_call)
	insist$must_be_grequal_than(num, 0, invoking_call)

	insist$must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)

	.count <- 0

	do.call( 'function', list(
		as.pairlist(xFormalsOf(fn)),
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
