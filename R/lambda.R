
#' Syntactic sugar for creating unary functions.

#' @param formal a symbol or string.
#' @param body a valid function body, which 
#'	 will be lazily evaluated.
#'
#' @return returns a unary function.

#' @export

':=' <- function (formal, body) {
	# symbol -> any -> function
	# construct a function from a symbol and
	# a function body.

	pcall <- sys.call()
	pframe <- parent.frame()
	formal <- match.call()[-1]$formal
	body <- match.call()[-1]$body

	assert(is.name(formal), pcall)

	lambda <- function () {}
	body(lambda) <- body
	# ------ make f a default-free unary function ------
	
	formals(lambda) <- 
		structure(
			list(quote(expr=)),
			names = match.call()[-1]$formal)

	# ------ make sure lexical scoping works is as expected ------
	environment(lambda) <- pframe
	lambda
}
