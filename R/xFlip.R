
#' xFlip
#'
#' Reverse the formal parametres of a function.
#'
#' @param
#'    fn an arbitrary function.
#'
#' @return
#'    a function of the same arity as \code{fn}.
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

	assert(
		is_fn_matchable(fn), invoking_call,
		exclaim$must_be_matchable(
			fn, summate(fn)) )

	fn <- match.fun(fn)
	remove(invoking_call)

	do.call('function', list(
		as.pairlist(rev( xFormals(fn) )),
		body(fn)
	))
}

