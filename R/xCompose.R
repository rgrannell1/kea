
#' xCompose
#' 
#' Compose two functions. 
#'
#' @param fn1 a unary function, or a 
#'	 symbol or name identifying such a function.
#' @param fn2 a unary function, or a 
#'	 symbol or name identifying such a function.
#'
#' @details Unlike some other functions that return functions in Arrow,
#'	 \code{xCompose} preserves the parameters of one of its input functions (the right function).
#'	 This is not always possible for some other functions, but it is included in
#'	 \code{xCompose} as the function is ubiquitous and should be as powerful as possible, even at
#'	 the risk of making Arrow less consistent.
#'
#' @return A function with the same parameters as \code{g}.
#'
#' @template glossary
#'
#' @examples 
#' @export

xCompose <- function (fn1, fn2) {
	# function -> function -> function
	# a general purpose compose function;
	# more poweful than \f.\g.\x.fgx
	
	pcall <- sys.call()

	require_a(traits$functionable, fn1, pcall)
	require_a(traits$functionable, fn2, pcall)
	
	fn1 <- match.fun(fn1)
	fn2 <- match.fun(fn2)
		
	remove(pcall)

	do.call("function", list(
		formals(fn2),
		bquote({
			fn1(.( call_with_params("fn2", fn2) ))
		})
	))
}

#' @export

'%of%' <- xCompose

#' @export

xQueer <- xCompose

#' @export

xQ <- xCompose
