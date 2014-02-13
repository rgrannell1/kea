
#' xFlip
#'
#' Reverse the formal parametres of a function.
#'
#' @param
#'    fn an arbitrary function. The function to have
#'    its formals reversed.
#'
#' @details
#'    \bold{xFlip} reverses the formal parametres of its
#'    input function, including the defaults of those arguments.
#'
#' @return
#'    A function of the same arity as \bold{fn}.
#'
#' @family function_modifying_functions
#'
#' @family parametre_functions
#'
#'
#' @example
#'    inst/examples/example-xFlip.R
#'
#' @rdname xFlip
#' @export

xFlip <- function (fn) {
	# function -> function
	#' reverse the parametres of a function.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(fn)

	insist $ must_be_fn_matchable(fn, invoking_call)

	fn <- match_fn(fn)
	fn <- xAsClosure(fn)

	remove(invoking_call)

	do.call('function', list(
		as.pairlist(rev( xFormalsOf(fn) )),
		body(fn)
	))
}

