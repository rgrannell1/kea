
#' Get the parameter names of a function or primitive function.
#'
#' @param fn An arbitrary function, or a 
#'     symbol or str identifying such a function.
#' @return A character vector.
#' @section Corner Cases:
#'     If \code{f} is a primitive function a slightly clumsier method of obtaining 
#'     formal arguments is used. If a parameter has no default, the value of the 
#'     corresponding element in the retun value will be the empty symbol, 
#'     which is identical to \code{quote(expr=)}.
#'
#' @family arrow-parameters
#' @export

#| function: xParameters version: 0.1 finished: false 

xParameters <- function (fn) {
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
