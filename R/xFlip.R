
#' xFlip
#'
#' Reverse the formal parametres of a function.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @return
#'    A function of the same arity as \code{fn}.
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#' @rdname xFlip
#' @export

xFlip <- function (fn) {
	# function -> function
	#' reverse the parametres of a function.

	invoking_call <- sys.call()

	assert(
		!missing(fn), invoking_call,
		exclaim$parametre_missing(fn))

	insist$must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)
	fn <- xAsClosure(fn)

	remove(invoking_call)

	do.call('function', list(
		as.pairlist(rev( xFormalsOf(fn) )),
		body(fn)
	))
}

