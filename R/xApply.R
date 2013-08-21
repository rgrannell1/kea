
#' Apply a list of arguments to a function
#'
#' @param f an arbitrary function, or a 
#'     symbol or name identifying such a function. 
#' @param collection a list or pairlist. The collection may be named, but
#'    all names must be parameters of \code{f}.
#'
#' @return the result of caling \code{f} with \code{collection}.
#'
#' @family arrow-apply
#' @export

#| function: xApply version: 0.1 finished: false 

xApply <- function (f, collection) {
	# function -> [any] -> any
	# call the function f with the list collection.

	pcall <- sys.call()
	pframe <- parent.frame()
	
	require_a("functionable", f, pcall)
	require_a(c("list", "pairlist"), collection, pcall)

	f <- match.fun(f)

	eval(
		as.call(c(f, collection)),
		envir = pframe)
}

#' @export

"%$%" <- xApply
