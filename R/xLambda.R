
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

relist <- function (expr) {

	if (is.name(expr)) {
		expr
	} else if (is.call(expr)) {
		lapply(expr, relist)
	} else {

		message <- 'invalid parametre'
		throw_exception $ syntax_error(lambda_call(), message)

	}
}

validate <- function (coll) {

	if (is.list(coll)) {


		message <- if (length(coll) != 3) {

			"invalid syntax."

		} else if (paste( coll[[1]]) != ':') {

			"parametres must be seperated by ':' delimiters."

		} else if (!is.symbol( coll[[3]] )) {

			"invalid parametre."

		}

		if (length(message) > 0) {
			throw_exception(lambda_call(), message)
		}

		validate( coll[[2]] )

	}

}

extract <- function (coll, params = character(0)) {

	if (is.list( coll[[2]] )) {
		extract( coll[[2]], c(paste( coll[[3]] ), params) )
	} else {
		parametres <- c(paste( coll[[2]] ), paste( coll[[3]] ), params)

		if (length( which(duplicated(parametres)) ) != 0) {

			duplicates <- vapply(which(duplicated(parametres)), ith_suffix, character(1))

			message    <-
				"duplicated parametres at " %+% toString(duplicates) %+%
				pluralise(" position", length(duplicates)) %+% "."

			throw_exception $ syntax_error(lambda_call(), message)

		}

		parametres
	}

}

lambda_call <- function () {

	sys_call      <- sys.call(1)[-1]
	invoking_call <- call( '%:=%', sys_call[[1]], sys_call[[2]] )

}





xLambda <- function (sym, val) {

	param_expr <- substitute(sym)
	body_expr  <- substitute(val)

	if (is.name(param_expr)) {
		# -- creating a unary function (more efficient).

		lambda              <- do.call('function', list(as_formals(paste(param_expr)), body_expr))
		environment(lambda) <- parent.frame()
		lambda

	} else if (is.call(param_expr)) {
		# -- need to crawl through the expression and pull out symbols.

		if ( param_expr[[1]] != '(') {

			message <- "the formals for non-unary functions " %+%
				"must be enclosed in parentheses."

			throw_exception $ syntax_error(lambda_call(), message)

		}

		sexp                <- relist( param_expr[[2]] )
		validate(sexp)

		parametres          <- extract(sexp)

		lambda              <- function () {}

		formals(lambda)     <- as_formals(parametres)
		body(lambda)        <- body_expr
		environment(lambda) <- parent.frame()

		lambda

	} else {

		message <- "invalid syntax."
		throw_exception $ syntax_error(lambda_call(), message)

	}
}

#' @rdname xLambda
#' @export

':=' <- xLambda
