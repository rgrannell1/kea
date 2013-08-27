
#' Compose two function functions.
#'
#' @param fn1 a unary function, or a 
#'     symbol or name identifying such a function.
#' @param fn2 a unary function, or a 
#'     symbol or name identifying such a function.
#'
#' @return A function with the same parameters as \code{g}.
#'
#' @export

#| function: xCompose version: 0.1 finished: false 

xCompose <- xAutoPartial(function (fn1, fn2) {
	# function -> function -> function
	# a general purpose compose function;
	# more poweful than \f.\g.\x.fgx
	
	pcall <- sys.call()
	pframe <- parent.frame()

	require_a("functionable", fn1, pcall)
	require_a("functionable", fn2, pcall)
	
	fn1 <- match.fun(fn1)
	fn2 <- match.fun(fn2)
		
	remove(pcall, pframe)

	do.call("function", list(
		formals(fn2),
		bquote({
			fn1(.( call_with_params("fn2", fn2) ))
		})
	))
})

#' @export

'%of%' <- xCompose
