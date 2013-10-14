
#' xCompose
#' 
#' Compose two functions. 
#'
#' @param fn1 a unary function.
#' @param fn2 a unary function.
#'
#' @details Unlike some other functions that return functions in Arrow,
#'	 \code{xCompose} preserves the parameters of one of its input functions (the right function).
#'	 This is not always possible for some other functions, but it is included in
#'	 \code{xCompose} as the function is ubiquitous and should be as powerful as possible, even at
#'	 the risk of making Arrow less consistent.
#'
#' @return A function with the same parameters as \code{fn2}.
#'
#' @template glossary
#'
#' @family higher_order_function
#'
#' @example inst/examples/blank.R
#' @export

xCompose <- function (fn1, fn2) {
	# function -> function -> function
	# a general purpose compose function;
	# more poweful than \f.\g.\x.fgx
	
	pcall <- sys.call()

	assert(
		!missing(fn1), pcall,
		exclaim$parameter_missing(fn1))

	assert(
		!missing(fn2), pcall,
		exclaim$parameter_missing(fn2))

	assert(
		is_fn_matchable(fn1), pcall,
		exclaim$must_be_matchable(fn1))
	
	assert(
		is_fn_matchable(fn2), pcall,
		exclaim$must_be_matchable(fn2))
	
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
