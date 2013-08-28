
#' xApply
#' 
#' Call a function with a list of arguments.
#'
#' @param fn an function of any arity.
#' @param coll a list or pairlist. This may be named, but
#'	all names must be parameters of \code{fn}.
#'
#' @return the return value of \code{fn}.
#'
#' @template glossary
#'
#' @examples 
#' @export

#| function: xApply version: 0.1 finished: false 

xApply <- xAutoPartial(function (fn, coll) {
	# function -> [any] -> any
	# call the function f with the list coll.

	pcall <- sys.call()
	pframe <- parent.frame()
	
	require_a("functionable", fn, pcall)
	require_a(c("list", "pairlist"), coll, pcall)

	fn <- match.fun(fn)

	eval(
		as.call(c(fn, coll)),
		envir = pframe)
})

#' @export

"%$%" <- xApply
