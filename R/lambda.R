
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

	pcall <- "xLambda:"

	say <- list(
		bad_symbol = 
			function (call, unexpected) {
				paste(call, "unexpected symbol in", paste0(unexpected, collapse = ""))
			},
		bad_param =
			function (call, unexpected, which) {

				paste(
					call, "unexpected", 
					paste0(class(unexpected), collapse = ""), 
					"constant at the", ith_suffix(which), "parameter")
			}
	)

	new_fn <- function () {}
	body(new_fn) <- body
	
	if (is.name(formals)) {
		# ------ make f a default-free unary function ------

		formals(new_fn) <- 
			structure(
				list(quote(expr=)),
				names = match.call()[-1]$formals)

	} else {
		# ------ try parse the bracket-enclosed formals ------

		params <- character(0)
		parse_tree <- formals
		delim <- ':'

		if (!paste( parse_tree[[1]] ) == '(') {
			stop(
				say$bad_symbol( pcall, parse_tree[[1]] ), call. = False )
		}

		# ------ remove the formal contents from the brackets
		parse_tree <- parse_tree[[2]]

		while (is.call(parse_tree) || is.name(parse_tree)) {

			param_ith <- length(params) + 1

			if (is.name(parse_tree)) {
				# ------ we've looped through the parse tree to the last symbol

				formal <- paste(parse_tree)
				parse_tree <- Null

			} else {
				# ------ check that each parameter is delimited using colons

				subtree <- list(
					delim = paste( parse_tree[[1]] ),
					rest = parse_tree[[2]],
					param = parse_tree[[3]]
				)

				if (!subtree$delim == delim) {
					stop(
						say$bad_symbol(pcall, subtree$param),
						call. = False )
				} else {
					
					if (!is.name(subtree$param)) {
						stop(
							say$bad_param(
								pcall, subtree$param, param_ith),
								call. = False)
					} else {
						parse_tree <- subtree$rest
						formal <- paste(subtree$param)
					}
				}
			}

			params <- c(formal, params)
		}

		# ------ set the formals to the parsed param names ------
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
