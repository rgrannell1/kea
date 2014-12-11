
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

# relist
#
#

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
			throw_exception $ syntax_error(lambda_call(), message)
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





xLambda <- local({

	LAMBDA        <- function () {}
	SINGLE_FORMAL <- list(quote(expr=))

	function (sym, val) {

		PARAM <- substitute(sym)
		BODY  <- substitute(val)

		if (is.name(PARAM)) {
			# -- creating a unary function (fast parentheses).
			# -- much more efficient than calling `function` with do.call.

			names(SINGLE_FORMAL) <- paste0(PARAM)

			formals(LAMBDA)      <- SINGLE_FORMAL
			body(LAMBDA)         <- BODY

			LAMBDA

		} else if (is.call(PARAM)) {
			# -- need to crawl through the expression and pull out symbols.

			if ( PARAM[[1]] != '(') {

				message <- "the formals for non-unary functions " %+%
					"must be enclosed in parentheses."

				throw_exception $ syntax_error(lambda_call(), message)

			}

			if (length( PARAM[[2]] ) == 1) {
				# -- unary function in braces.
				# -- this path should be factored out in the future.

				if (!is.name( PARAM[[2]] )) {

					message <- "invalid parametre."

					throw_exception $ syntax_error(lambda_call(), message)

				}

				names(SINGLE_FORMAL) <- paste0( PARAM[[2]] )

				formals(LAMBDA)      <- SINGLE_FORMAL
				body(LAMBDA)         <- BODY

				return(LAMBDA)

			}

			sexp                <- relist( PARAM[[2]] )
			validate(sexp)

			formals(LAMBDA)     <- as_formals(extract(sexp))
			body(LAMBDA)        <- BODY

			LAMBDA

		} else {

			message <- "invalid syntax."
			throw_exception $ syntax_error(lambda_call(), message)

		}
	}

})

#' @rdname xLambda
#' @export

':=' <- xLambda
