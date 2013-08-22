
#' Construct a unary function from a parameter and function body.
#'
#' @param formal a single symbol or string giving the parameter name
#'     of the function to be returned.
#' @param body a valid function body.
#'
#' @return a unary function.
#' @export

#| function: %=>% version: 0.1 finished: false 

'%=>%' <- function (formal, body) {
	# shorthand constructor for unary functions.

	pcall <- sys.call()
	pframe <- parent.frame()
	args <- as.list( match.call() )[-1]

	require_a(c("symbol", "string"), args$formal, pcall)
	require_a("any", args$body, pcall)

	f <- eval( bquote(function () .(args$body)) )
	
	formals(f) <- 
		structure(
			list(quote(expr=)),
			names = toString(args$formal)
		)

	environment(f) <- pframe
	f
}
