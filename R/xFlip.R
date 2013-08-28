
#' xFlip
#' 
#' Reverse the formal parametres of a function.
#'
#' @param fn an arbitrary function.
#'
#' @return a function of the same arity as \code{fn}.
#'
#' @template glossary
#'
#' @examples 
#' @export

xFlip <- function (fn) {
	# function -> function
	#' reverse the parameters of a function.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)

	fn <- match.fun(fn)
	remove(pcall)

	do.call('function', list(
		as.pairlist(rev( xFormals(fn) )),
		body(fn)
	))
}
