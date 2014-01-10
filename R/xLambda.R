
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
#'    Returns a unary function.
#'
#' @rdname xLambda
#' @export

xLambda <- local({

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
		rest =
			function (tree, symbol = True) {
				(if (symbol) identity else paste)( tree[[2]] )
			},
		param =
			function (tree, symbol = True) {
				(if (symbol) identity else paste)( tree[[3]] )
			}
	)

	function (sym, val) {
		# symbol -> any -> function
		# construct a function from a symbol and
		# a function body.

		parent_frame <- parent.frame()
		sym <- match.call()$sym
		val <- match.call()$val

		invoking_call <- "xLambda:"

		lambda <- function () {}

		body(lambda) <- val

		make_formals <- function (names) {
			structure(
				rep(list(quote(expr=)), length(names)),
				names = names)
		}

		if (is.name(sym)) {
			# ------ make lambda a default-free unary function ------ #

			formals(lambda) <- make_formals(sym)

		} else {
			# ------ try parse the bracket-enclosed formals ------ #

			collect_params <- function (tree, state) {
				# recur into the formals parse tree, accumulating
				# parametre names and validating the tree.

				if (is.name(tree)) {

					c(paste(tree), state$params)

				} else if (is.call(tree)) {

					assert(
						get$delim(tree) == token$delim(), invoking_call,
						proclaim$incorrent_delimiter(
							state$pos, token$delim(False)) )

					assert(
						is.name(get$param(tree)), invoking_call,
						proclaim$non_symbol_param(state$pos + 1))

					new_state <- list(
						pos =
							state$pos + 1,
						params =
							c(get$param(tree, True), state$params) )

					collect_params(
						get$rest(tree),
						new_state)

				}
			}


			# ------ check the formals are bracket-enclosed ------ #

			assert(
				get$delim(sym) == token$open(), invoking_call,
				proclaim$no_enclosing_parens())

			params <- collect_params(
				tree = sym[[2]],
				state = list(
					pos = 1,
					params = character(0)) )

			# ------ set the formals to the parsed param names ------ #
			formals(lambda) <-make_formals(params)

		}

		# ------ make sure lexical scoping works is as expected ------ #
		environment(lambda) <- parent_frame
		lambda
	}
})

#' @rdname xLambda
#' @export

':=' <- xLambda
