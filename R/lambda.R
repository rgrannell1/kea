
#' Syntactic sugar for creating unary functions.

#' @param formals a symbol or string.
#' @param body a valid function body, which 
#'	 will be lazily evaluated.
#'
#' @return returns a unary function.

#' @export

xLambda <- function (formals, body) {
	# symbol -> any -> function
	# construct a function from a symbol and
	# a function body.

	pframe <- parent.frame()
	formals <- match.call()$formals
	body <- match.call()$body

	pcall <- "xLambda:"

	new_fn <- function () {}
	body(new_fn) <- body
	
	if (is.name(formals)) {
		# ------ make f a default-free unary function ------

		formals(new_fn) <- 
			structure(
				list(quote(expr=)),
				names = match.call()[-1]$formals)

	} else {
		# ------ try parse the bracket-enclosed formals ------

		collect_params <- function (tree, state) {
			# recur into the formals parse tree, accumulating 
			# parameter names and validating the tree.

			if (is.name(tree)) {
				c(paste(tree), state$params)
			} else {

				if (get$delim(tree) != token$delim()) {
					# ------ the parameters aren't delimited with ":" ------

					msg <- pcall + 
						" the " + ith_suffix(state$pos) + 
						" delimiter should be " + 
						dQuote(token$delim(False)) + "."

					stop (msg, call. = False)
				}

				if ( !is.name(get$param(tree)) ) {
					# ------ the parameter name is invalid ------

					msg <- pcall + 
						" the " + ith_suffix(state$pos + 1) + 
						" parameter is a non-symbol."

					stop (msg, call. = False)
				}

				new_state <- list(
					pos = 
						state$pos + 1,
					params = 
						c(get$param(tree, True), state$params) )

				collect_params(get$rest(tree), new_state)

			}
		}

		# ------ tokens of particular importance ------

		token <- list(
			open = 
				function (symbol = True) {
					(if (symbol) as.symbol else paste)( "(" )
				},
			delim =
				function (symbol = True) {
					(if (symbol) as.symbol else paste)( ":" )
				}
		)

		# ------ grab different parts of the parse tree ------

		get <- list(
			delim = 
				function (tree, symbol = True) {
					(if (symbol) identity else paste)( tree[[1]] )
				},
			param = 
				function (tree, symbol = True) {
					(if (symbol) identity else paste)( tree[[3]] )
				},
			rest = 
				function (tree, symbol = True) {
					(if (symbol) identity else paste)( tree[[2]] )			
				}
		)

		# ------

		if (get$delim(formals) != token$open()) {
		
			msg <- pcall + " the formals for non-unary functions" + 
				" must be enclosed in parentheses."

			stop (msg, call. = False)
		}

		params <- collect_params(
			tree = formals[[2]], 
			state = list(pos = 1, params = character(0)) )

		# ------ set the formals to the parsed param names ------
		formals(new_fn) <- 
			structure(
				rep(list(quote(expr=)), length(params)),
				names = params)

	}

	# ------ make sure lexical scoping works is as expected ------
	environment(new_fn) <- pframe
	new_fn
}

#' @export

':=' <- xLambda
