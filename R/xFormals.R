
#' Get the parameters and defaults of a function or primitive function.
#'
#' @param f An arbitrary function, or a 
#'     symbol or string identifying such a function.
#' @return A named list, where each element's name is a parameter in f and each value
#'     is the default value of that parameter.
#' @section Corner Cases:
#'     If \code{f} is a primitive function a slightly clumsier method of obtaining 
#'     formal arguments is used. If a parameter has no default, the value of the 
#'     corresponding element in the retun value will be the empty symbol, 
#'     which is identical to \code{quote(expr=)}.
#'
#' @family arrow-parameters
#' @export

#| function: xFormals version: 0.1 finished: false 

xFormals <- function (f) {
	# (a -> b) -> [a, b]
	# get the formals of non-primitive functions, and
	# the arguments of primitive functions.

	pcall <- sys.call()
	require_a("functionable", f, pcall)

	f <- match.fun(f)

	if (is.primitive(f)) {
		as.list( head(as.list(args(f)), -1) )
	} else {
		as.list( formals(f) )
	}
}
