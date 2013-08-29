
#' xParams
#' 
#' Get the parametre names of a function or primitive function.
#'
#' @param fn an arbitrary function or primitive function.
#'
#' @return a character vector/.
#'
#' @section Corner Cases:
#'	 If \code{fn} is a primitive function a heuristic is used to obtain 
#'	 its parameter names.
#' @template glossary
#'
#' @examples 
#' @export

#| function: xParams version: 0.1 finished: false 

xParams <- function (fn) {
	# function -> Vector string
	# get the formals of non-primitive functions, and
	# the arguments of primitive functions.

	pcall <- sys.call()
	require_a("functionable", fn, pcall)

	fn <- match.fun(fn)

	formals_fn <- if (is.primitive(fn)) {
		as.list( head(as.list(args(fn)), -1) )
	} else {
		as.list( formals(fn) )
	}

	if (length(formals_fn) == 0) {
		character(0)
	} else {
		names(formals_fn)
	}
}

#' @export

xParams <- xParams
