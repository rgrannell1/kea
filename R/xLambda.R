
#' xLambda
#'
#' Syntactic sugar for creating functions.
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

xLambda <- local({

	# -- grab different parts of the abstract syntax tree.

	get_tree <- list(
		delim =
			function (tree, symbol = True) {

				if (symbol) {
					tree[[1]]
				} else {
					paste( tree[[1]] )
				}

			},
		rest =
			function (tree, symbol = True) {

				if (symbol) {
					tree[[2]]
				} else {
					paste( tree[[2]] )
				}

			},
		param =
			function (tree, symbol = True) {

				if (symbol) {
					tree[[3]]
				} else {
					paste( tree[[3]] )
				}

			}
	)

	# -- try parse the bracket-enclosed formals

	collect_params <- function (tree, state, invoking_call = sys.call(1)) {
		# -- recur into the formals parse tree, accumulating
		# -- parametre names and validating the tree.

		if (is.name(tree)) {

			c(paste(tree), state $ params)

		} else if (is.call(tree)) {

			if ( !is.name(get_tree $ param(tree)) ) {
				# -- the parametre isn't a symbol

				message <-
					"function parametres must be symbols." %+%
					summate(get_tree $ param(tree))

				invoking_call <- call(
					'%:=%',
					invoking_call[-1][[1]],
					invoking_call[-1][[2]])

				throw_kea_error(invoking_call, message)
			}

			if (get_tree $ delim(tree) != ":") {

				message <-
					"parametres must be delimited by ':'"

				invoking_call <- call(
					'%:=%',
					invoking_call[-1][[1]],
					invoking_call[-1][[2]])

				throw_kea_error(invoking_call, message)
			}

			new_state <- list(
				params =
					c(paste(get_tree $ param(tree, True)), state $ params) )

			collect_params(get_tree $ rest(tree), new_state)

		}
	}

	brace <- as.symbol('{')

	function (sym, val) {
		# symbol -> any -> function
		# -- construct a function from a symbol and
		# -- a function body.

		param_block <- substitute(sym)
		val         <- substitute(val)

		if (is.name(param_block)) {
			# -- make lambda a default-free unary function.

			param_block <- paste(param_block)
			# -- fast track.

			lambda <- do.call('function', list(
				as.pairlist(as_formals(param_block)),
				val
			))

			lambda <- MakeFun(character(0), lambda, False)

			environment(lambda) <- parent.frame()

			lambda

		} else {

			if (get_tree $ delim(param_block) != '(') {
				# -- check the formals are bracket-enclosed

				message <-
					"the formals for non-unary functions" %+%
					" must be enclosed in parentheses."

				invoking_call <- call(
					'%:=%',
					sys.call()[-1][[1]],
					sys.call()[-1][[2]])

				throw_kea_error(invoking_call, message)
			}

			params <- paste(collect_params( param_block[[2]], list(params = character(0)) ))
			which_duplicated <- which(duplicated(params))

			if (length(which_duplicated) > 0) {

				message <-
					"parametres must not be duplicated: " %+% toString(params[which_duplicated])

				invoking_call <- call(
					'%:=%',
					sys.call(1)[-1][[1]],
					sys.call(1)[-1][[2]])

				throw_kea_error(invoking_call, message)
			}
			# -- this is just a normal R function; map one-to-one onto
			# -- R code.

			lambda <- function () {}

			formals(lambda)     <- as_formals(params)
			body(lambda)        <- val
			lambda              <- MakeFun(character(0), lambda, False)

			environment(lambda) <- parent.frame()

			lambda
		}
	}
})

#' @rdname xLambda
#' @export

':=' <- xLambda
