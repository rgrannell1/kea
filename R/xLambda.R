
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
#' @export

xLambda <- local({

	# -- grab different parts of the parse tree

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

	function (sym, val) {
		# symbol -> any -> function
		# construct a function from a symbol and
		# a function body.

		parent_frame <- parent.frame()
		matched <- match.call()

		sym <- matched$sym
		val <- matched$val

		# -- will always be length > 0, but may deparse badly if
		# -- the formals aren't symbols, so use selectively.
		invoking_call <- paste0(ddparse( matched[-1][[1]] ), ' := { [truncated]')

		lambda <- function () {}

		body(lambda) <- val

		if (is.name(sym)) {
			# -- make lambda a default-free unary function

			formals(lambda) <- make_formals(sym)

		} else {
			# -- try parse the bracket-enclosed formals

			collect_params <- function (tree, state) {
				# -- recur into the formals parse tree, accumulating
				# -- parametre names and validating the tree.

				if (is.name(tree)) {

					c(paste(tree), state$params)

				} else if (is.call(tree)) {

					if ( !is.name(get_tree$param(tree)) ) {
						# -- the parametre isn't a symbol

						message <- "function parametres must by symbols." %+%
						summate(get_tree$param(tree))

						throw_arrow_error(invoking_call, message)
					}

					if (get_tree$delim(tree) != ":") {

						message <- "parametres must be delimited by ':'"

						throw_arrow_error(invoking_call, message)
					}

					new_state <- list(
						params =
							c(get_tree$param(tree, True), state$params) )

					collect_params(
						get_tree$rest(tree), new_state)

				}
			}

			# -- check the formals are bracket-enclosed

			if (get_tree$delim(sym) != '(') {

				message <- "the formals for non-unary functions" %+%
					" must be enclosed in parentheses."

				throw_arrow_error(invoking_call, message)
			}

			params <- collect_params(
				tree = sym[[2]],
				state = list(
					params = character(0)) )

			# -- set the formals to the parsed param names
			formals(lambda) <- make_formals(params)
		}

		# -- set the environment to exclude all the clutter in this function
		environment(lambda) <- parent_frame
		lambda
	}
})

#' @rdname xLambda
#' @export

':=' <- xLambda
