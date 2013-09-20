
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

	pcall <- paste(
		paste0(deparse(formals), collapse = ''), ":= { }"  )

	new_fn <- function () {}
	body(new_fn) <- body
	
	if (is.name(formals)) {
		# ------ make f a default-free unary function ------

		formals(new_fn) <- 
			structure(
				list(quote(expr=)),
				names = match.call()[-1]$formals)

	} else {
		# ------ try parse the formals ------

		params <- c()
		parse_tree <- formals
		delim <- ':'


		if (!paste( parse_tree[[1]] ) == '(') {
			stop(pcall, ":", 'unexpected symbol in ', parse_tree[[1]], call. = False )
		}

		parse_tree <- parse_tree[[2]]

		while (is.call(parse_tree) || is.name(parse_tree)) {

			if (is.name(parse_tree)) {
				# at the end of the parse tree

				formal <- paste(parse_tree)
				parse_tree <- Null

			} else {
				# check that params are | delimitied

				subtree <- list(
					delim = paste( parse_tree[[1]] ),
					rest = parse_tree[[2]],
					param = parse_tree[[3]]
				)

				if (!subtree$delim == delim) {
					stop(pcall, ":", 'unexpected symbol in ',subtree$param, call. = False)
				} else {
					if (!is.name(subtree$param)) {
						stop(pcall, ":", "non-symbol following ':'", call. = False)
					} else {

						parse_tree <- subtree$rest
						formal <- paste(subtree$param)
					}					
				}
			}

			params <- c(formal, params)
		}

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
