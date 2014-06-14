
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

				message <-
					"function parametres must by symbols." %+%
					summate(get_tree $ param(tree))

				invoking_call <- paste0(
					ddparse( match.call()[-1][[1]] ),
					' := { [truncated]')

				throw_kiwi_error(invoking_call, message)
			}

			if (get_tree $ delim(tree) != ":") {

				message <-
					"parametres must be delimited by ':'"

				invoking_call <- paste0(
					ddparse( match.call()[-1][[1]] ),
					' := { [truncated]')

				throw_kiwi_error(invoking_call, message)
			}

			new_state <- list(
				params =
					c(paste(get_tree $ param(tree, True)), state $ params) )

			collect_params(get_tree $ rest(tree), new_state)

		}
	}

	construct_function <- function (params, exprbody, env, invoking_call = sys.call(1)) {
		# -- create a function from parametres, body and an environment.
		# -- underscored parametres write code into the function body.

		which_duplicated <- which(duplicated(params))

		if (length(which_duplicated) > 0) {

			message <-
				"parametres must not be duplicated: " %+% toString(params[which_duplicated])

			invoking_call <-
				paste0(ddparse( invoking_call[-1][[1]] ), ' := { [truncated]')

			throw_kiwi_error(invoking_call, message)
		}


		lambda <- function () {}

		is_underscored <- grepl('_$', params)

		if (!any(is_underscored)) {
			# -- this is just a normal R function; map one-to-one onto
			# -- R code.

			formals(lambda)     <- as_formals(params)
			body(lambda)        <- exprbody
			environment(lambda) <- env

		} else {
			# -- some parametres are underscored, so the matching argument
			# -- must be converted to an arrow object first.

			final_params     <- params

			# -- throws a runtime error if a parametre ends up duplicated.
			final_params[is_underscored] <-
				substr(params[is_underscored], 1, nchar(params[is_underscored]) - 1)


			which_duplicated <- which(duplicated(final_params))

			if (length(which_duplicated) > 0) {
				# -- duplicated upon truncation.

				message <-
					"parametres must not be duplicated when the final underscore is removed: " %+% toString(final_params[which_duplicated])

				invoking_call <-
					paste0(ddparse( invoking_call[-1][[1]] ), ' := { [truncated]')

				throw_kiwi_error(invoking_call, message)

			}

			# construct assignments of the form 'a_ <- x_(a)'.
			kiwi_assignments <- lapply(which(is_underscored), function (ith) {

				param       <- params[[ith]]
				final_param <- final_params[[ith]]

				bquote( .(as.symbol(param)) <- x_( .(as.symbol(final_param)) ) )
			})

			formals(lambda)     <- as_formals(final_params)
			body(lambda)        <- join_exprs(kiwi_assignments, exprbody)
			environment(lambda) <- env
		}
		lambda
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

			construct_function(paste(param_block), val, parent.frame())

		} else {

			if (get_tree $ delim(param_block) != '(') {
				# -- check the formals are bracket-enclosed

				message <-
					"the formals for non-unary functions" %+%
					" must be enclosed in parentheses."

				invoking_call <-
					paste0(ddparse( match.call()[-1][[1]] ), ' := { [truncated]')

				throw_kiwi_error(invoking_call, message)
			}

			params <- collect_params( param_block[[2]], list(params = character(0)) )

			construct_function(paste(params), val, parent.frame())

		}
	}
})

#' @rdname xLambda
#' @export

':=' <- xLambda
