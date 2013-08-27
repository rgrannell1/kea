
#' Apply a list of arguments to a function
#'
#' @param fn an arbitrary function, or a 
#'     symbol or name identifying such a function. 
#' @param coll a list or pairlist. The collection may be named, but
#'    all names must be parameters of \code{fn}.
#'
#' @return the result of caling \code{fn} with \code{coll}.
#'
#' @family arrow-apply
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
