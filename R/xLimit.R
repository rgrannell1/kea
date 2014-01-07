
#' xLimit
#'
#' Create a function that can call its underlying
#' function a limited number of times.
#'
#' @section Uses:
#'
#'    \code{xLimit} returns a function that behaves
#'    identically to the original input function, but
#'    has a limited lifespan. A natural application of
#'    \code{xLimit} is for creating functions that interact
#'    with web API's; most web API's only allow users to make
#'    a preset number of requests per hour. \code{xLimit} could
#'    be used to create functions with a built-in rate limit, to
#'    avoid having to writing rate-limit checking boilerplate.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @param
#'    num a positive whole number.
#'
#' @return
#'    A function with the same parametres as \code{fn}.
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

	num <- as_typed_vector(num, 'numeric', True)

	assert(
		length(num) %in% 0:1, invoking_call,
		exclaim$must_have_length(
			num, 0:1, summate(num)) )

	assert(
		num > 0, invoking_call,
		exclaim$must_be_whole(
			num, summate(num)) )

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	fn <- match.fun(fn)

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
