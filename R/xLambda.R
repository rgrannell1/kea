
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

	collect_params <- function (tree, state) {
		# -- recur into the formals parse tree, accumulating
		# -- parametre names and validating the tree.

		if (is.name(tree)) {

			c(paste(tree), state $ params)

		} else if (is.call(tree)) {

			if ( !is.name(get_tree $ param(tree)) ) {
				# -- the parametre isn't a symbol

				message <- "function parametres must by symbols." %+%
				summate(get_tree $ param(tree))

				invoking_call <- paste0(ddparse( match.call()[-1][[1]] ), ' := { [truncated]')
				throw_kiwi_error(invoking_call, message)
			}

			if (get_tree $ delim(tree) != ":") {

				message <- "parametres must be delimited by ':'"

				invoking_call <- paste0(ddparse( match.call()[-1][[1]] ), ' := { [truncated]')
				throw_kiwi_error(invoking_call, message)
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

		sym <- substitute(sym)
		val <- substitute(val)

		lambda <- function () {}

		if (is.name(sym)) {
			# -- make lambda a default-free unary function

			param <- paste(sym)
			is_underscored <- grepl('.+_$', param)

			if (!any(is_underscored)) {
				# -- the parametres can be used as is.
				formals(lambda) <- as_formals(param)
				body(lambda)    <- val
			} else {

				# -- set the formals to the parsed param names

				kiwised <- lapply(param[is_underscored], function (param) {
					# -- remove the underscore from the parametre name.

					final_param <- substr(param, 1, nchar(param) - 1)

					# -- create a kiwi-assignment operator.
					bquote( .(as.symbol(param)) <- x_( .(as.symbol(final_param)) ) )
				})

				boilerplated_body <- join_exprs(kiwised, val)

				# -- remove the underscores from the paramtre names
				# -- before setting.
				formals(lambda) <-
					as_formals(substr(param, 1, nchar(param) - 1))

				body(lambda) <- boilerplated_body
			}



		} else {

			# -- check the formals are bracket-enclosed

			if (get_tree $ delim(sym) != '(') {

				message <- "the formals for non-unary functions" %+%
					" must be enclosed in parentheses."

				invoking_call <- paste0(ddparse( match.call()[-1][[1]] ), ' := { [truncated]')
				throw_kiwi_error(invoking_call, message)
			}

			params <- collect_params(
				tree  = sym[[2]],
				state = list(
					params = character(0)) )

			# -- does the parametre end in an underscore?
			is_underscored <- grepl('.+_$', params)

			if (!any(is_underscored)) {
				# -- the parametres can be used as is.
				formals(lambda) <- as_formals(params)
				body(lambda)    <- val
			} else {

				# -- set the formals to the parsed param names

				kiwised <- lapply(params[is_underscored], function (param) {
					# -- remove the underscore from the parametre name.

					final_param <- substr(param, 1, nchar(param) - 1)

					# -- create a kiwi-assignment operator.
					bquote( .(as.symbol(param)) <- x_( .(as.symbol(final_param)) ) )
				})

				boilerplated_body <- join_exprs(kiwised, val)

				# -- remove the underscores from the paramtre names
				# -- before setting.
				formals(lambda) <-
					as_formals(substr(params, 1, nchar(params) - 1))

				body(lambda)    <- boilerplated_body
			}
		}

		# -- set the environment to exclude all the clutter in this function
		environment(lambda) <- parent.frame()
		lambda
	}
})

#' @rdname xLambda
#' @export

':=' <- xLambda
