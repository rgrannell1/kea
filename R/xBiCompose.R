
#' Compose a binary function with functions as its left and right arguments.

#' @param fn1 
#' @param fn2 
#' @param fn3 

#' @export

xBiCompose <- xAutoPartial(function (fn1, fn2, fn3) {
	# the phoenix or Phi combinator

	pcall <- sys.call()
	pframe <- parent.frame()

	require_a("functionable", fn1, pcall)
	require_a("functionable", fn2, pcall)
	require_a("functionable", fn3, pcall)

	fn1 <- match.fun(fn1)
	fn2 <- match.fun(fn2)
	fn3 <- match.fun(fn3)

	function (x) {
		fn1( fn2(x), fn3(x) )
	}
})

#' @export

xPhoenix <- xBiCompose

#' @export

xSprime <- xPhoenix
