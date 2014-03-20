
#' xLambda
#'
#' Syntactic sugar for creating functions.
#'
#'
#' @details
#'     As of version 0.1.0 xLambda functions cannot have default parametres. This
#'     may change at a later date.
#'
#'     The parametre expression is of the form
#'
#'     \code{x}
#'
#'     Or
#'
#'     \code{(x)}
#'
#'     Or
#'
#'     \code{(x : y : z)}
#'
#'    Due to parser limitations parametres are colon-delimited, not comma delimited. Non-unary
#'    functions must enclose their parametres in parentheses.
#'
#'    The body of the function can optionally be enclosed in braces. The ':=' operator has very low
#'    preceedence, so sometimes parametres may be necessary for the correct function to be created.
#'
#'    \code{x := x^2 \%then\% x := x}
#'
#'    is interpreted as
#'
#'    \code{x := {x^2 \%then\% x := x}}
#'
#'    . In this case
#'
#'    \code{(x := x^2) \%then\% (x := x)}
#'
#'    is required to create the correct composed function.
#'
#' @param
#'    sym a parametre expression. The parametres to bind to
#'    to the function.
#'
#' @param
#'    val an expression. The body of the function.
#'
#' @return
#'    Returns a function.
#'
#' @example
#'    inst/examples/example-xLambda.R
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

	get_tree <- list(
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

	make_formals <- function (names) {
		structure(
			rep(list(quote(expr=)), length(names)),
			names = names)
	}

	function (sym, val) {
		# symbol -> any -> function
		# construct a function from a symbol and
		# a function body.

		parent_frame <- parent.frame()

		sym <- match.call()$sym
		val <- match.call()$val

		invoking_call <- as.symbol("xLambda")

		lambda <- function () {}

		body(lambda) <- val

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

					#insist $ must_have_correct_delimiter(
					#	get_tree, token, tree, invoking_call)

					assert(
						is.name(get_tree$param(tree)), invoking_call,
						proclaim$non_symbol_param(get_tree$param(tree)) )

					new_state <- list(
						params =
							c(get_tree$param(tree, True), state$params) )

					collect_params(
						get_tree$rest(tree), new_state)

				}
			}


			# ------ check the formals are bracket-enclosed ------ #

			assert(
				get_tree$delim(sym) == token$open(), invoking_call,
				proclaim$no_enclosing_parens())

			params <- collect_params(
				tree = sym[[2]],
				state = list(
					params = character(0)) )

			# ------ set the formals to the parsed param names ------ #
			formals(lambda) <- make_formals(params)

		}

		# ------ make sure lexical scoping works is as expected ------ #
		environment(lambda) <- parent_frame
		lambda
	}
})

#' @rdname xLambda
#' @export

':=' <- xLambda
