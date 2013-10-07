
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
#' @examples inst/examples/blank.R
#' @export

xFlip <- function (fn) {
	# function -> function
	#' reverse the parameters of a function.

	pcall <- sys.call()

	assert(
		!missing(fn), pcall, 
		exclaim$parameter_missing(fn))

	assert(
		is.function(fn) || is.symbol(fn) || 
		(is.character(fn) && length(fn) == 1), pcall, 
		exclaim$must_be_matchable(fn))
	
	fn <- match.fun(fn)
	remove(pcall)

	do.call('function', list(
		as.pairlist(rev( xFormals(fn) )),
		body(fn)
	))
}

#' @export

xCardinal <- xFlip

#' @export

xC <- xFlip
