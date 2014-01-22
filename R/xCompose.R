
#' xCompose
#'
#' Compose two functions.
#'
#' @param
#'    fns a collection of functions.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A function with the same parametres as \code{fn2}.
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
	# Collection function -> function

	invoking_call <- sys.call()

	insist$must_be_collection_(fns, invoking_call)
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

