
#' xCompose
#'
#' Compose two functions.
#'
#' @param
#'    fns a collection of functions.
#'
#' @return
#'    A function with the same parametres as \code{fn2}.
#'
#' @family higher_order_functions
#'
#' @family function_modifying_functions
#'
#' @family function_combining_functions
#'
#' @template
#'    Variadic
#'
#' @rdname xCompose
#' @export

xCompose <- function (fns) {
	# function -> function -> function
	# a general purpose compose function;
	# more poweful than \f.\g.\x.fgx

	invoking_call <- sys.call()

	assert(
		is_recursive(fns), invoking_call,
		exclaim$must_be_recursive(
			fns, profile_object(fns)) )

	assert(
		all(sapply(fns, is_fn_matchable)), invoking_call,
		exclaim$must_be_recursive_of_matchable(
			fns, profile_object(fns)) )

	fns <- lapply(fns, match.fun)

	function (...) {
		"a function created by xCompose."
		""
		invoking_call <- sys.call()

		init <- c(...)

		for (ith in seq_along(fns)) {

			fn <- fns[[ith]]
			init <- try_higher_order( fn(init), invoking_call )
		}

		init
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

