
#' Compose two function functions.
#'
#' @param fn_1 a unary function, or a 
#'     symbol or name identifying such a function.
#' @param fn_2 a unary function, or a 
#'     symbol or name identifying such a function.
#'
#' @return A function with the same parameters as \code{g}.
#'
#' @export
#'

#| function: xCompose version: 0.1 finished: false 

xCompose <- function (fn_1, fn_2) {
	# function -> function -> function
	# return a composite function f o g.
	# constructed using a macro.
	
	pcall <- sys.call()
	pframe <- parent.frame()

	require_a("functionable", fn_1, pcall)
	require_a("functionable", fn_2, pcall)
	
	fn_1 <- match.fun(fn_1)
	fn_2 <- match.fun(fn_2)
		
	remove(pcall, pframe)

	do.call("function", list(
		formals(fn_2),
		bquote({
			fn_1(.( call_with_params("fn_2", fn_2) ))
		})
	))
}

#' @export

'%of%' <- xCompose
