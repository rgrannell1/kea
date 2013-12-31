
#' xLambda
#'
#' Syntactic sugar for creating unary functions.
#'
#' @param
#'    sym a symbol or string.
#'
#' @param
#'    val a valid function body, which will be lazily evaluated.
#'
#' @return
#'    returns a unary function.
#'
#' @rdname xLambda
#' @export

xLambda <- function (sym, val) {
	# symbol -> any -> function
	# construct a function from a symbol and
	# a function body.

	parent_frame <- parent.frame()
	sym <- match.call()$sym
	val <- match.call()$val

	invoking_call <- "xLambda:"

	lambda <- function () {

	}

	body(lambda) <- val

	if (is.name(sym)) {
		# ------ make lambda a default-free unary function ------ #

		formals(lambda) <-
			structure(
				list(quote(expr=)),
				names = match.call()[-1]$sym)

	} else {
		# ------ try parse the bracket-enclosed formals ------ #

		collect_params <- function (tree, state) {
			# recur into the formals parse tree, accumulating
			# parametre names and validating the tree.

			if (is.name(tree)) {

				c(paste(tree), state$params)

			} else if (is.call(tree)) {

				if (get$delim(tree) != token$delim()) {
					# ------ the parametres aren't delimited with ":" ------ #

					msg <- invoking_call +
						" the " %+% ith_suffix(state$pos) %+%
						" delimiter should be " %+%
						dQuote(token$delim(False)) %+% "."

					stop (msg, call. = False)
				}

				if ( !is.name(get$param(tree)) ) {
					# ------ the parametre name is invalid ------ #

					msg <- invoking_call +
						" the " %+% ith_suffix(state$pos + 1) %+%
						" parametre is a non-symbol."

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

		# ------ tokens of particular importance ------ #

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

		# ------ grab different parts of the parse tree ------ #

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

		# ------ check the formals are bracket-enclosed ------ #

		if (get$delim(sym) != token$open()) {

			msg <- invoking_call %+% " the formals for non-unary functions" %+%
				" must be enclosed in parentheses."

			stop (msg, call. = False)
		}

		params <- collect_params(
			tree = sym[[2]],
			state = list(pos = 1, params = character(0)) )

		# ------ set the formals to the parsed param names ------ #
		formals(lambda) <-
			structure(
				rep(list(quote(expr=)), length(params)),
				names = params)

	}

	# ------ make sure lexical scoping works is as expected ------ #
	environment(lambda) <- parent_frame
	lambda
}

#' @rdname xLambda
#' @export

':=' <- xLambda
