
MakeFun <- function (expr) {

	parent_frame <- parent.frame()

	unquote <- function (inner) {

		if (is.pairlist(inner)) {
			as.pairlist(lapply(inner, unquote))
		} else if (length(inner) <= 1L) {
			inner
		} else if (inner[[1L]] == as.name("MACRO")) {
			eval(inner[[2L]], parent_frame)
		} else {
			as.call(lapply(inner, unquote))
		}
	}

	expr <- match.call() $ expr

	# -- evaluate the function, to allow access to its parts.
	fn     <- eval(unquote(expr), parent_frame)
	params <- names(formals(fn))

	# -- the function to ultimately return.
	boilerplated <- function () {}

	# -------------------------------- Fix Macro -------------------------------- #
	#
	# -- this macro calls a second macro, which injects partial application
	# -- into the returned function.

	fix_macro_call <- bquote(
		# -- no need to work about '...' in this case

		.(as.call( c(
			as.symbol('Fix'),

			# -- the function to return in a fixed form.
			call('sys.function'),

			# -- the parametres to bind over.
			lapply(params, as.symbol)


		) ))
	)

	formals(boilerplated)     <- formals(fn)
	body(boilerplated)        <- bquote({

		# -- all functions are partially applied.
		.(eval(fix_macro_call))

		# -- add the real function body after the boilerplate headers.
		.(body(fn))
	})

	environment(boilerplated) <- parent.frame()

	# -- now evaluate calls to MACRO
	# eval(unquote(expr), parent_frame)

	boilerplated
}











# MakeVariadic
#
# MakeVariadic takes a function, and the variable to fix, and it generates
# a variadic form of a function.

MakeVariadic <- function (fn, fixed) {

	env <- new.env(parent = environment(fn))

	fn_sym    <- as.symbol(match.call()$fn)
	varfn_sym <- as.symbol(paste0(fn_sym, '_'))

	if ( grepl('_', paste0(fn_sym)) ) {
		stop("MakeVariadic: _ in method name ", paste0(fn_sym))
	}

	# -- will replace formals & body, env will be same.
	out <- fn

	# -- will break if defaults are ever added to kiwi.

	params <- names(formals(fn))

	if (fixed %!in% params) {
		stop("MakeVariadic: tried to fix param that doesn't exist ", paste0(fn_sym))
	}

	params[params == fixed] <- '...'

	# -- create a formal list from the new parametres with no defaults.
	formals(out) <- as_formals(params)

	fix_macro_call <- bquote(

		.(as.call( c(
			as.symbol('Fix'),
			varfn_sym,
			lapply(params, function (param) {
				# -- needed to go through a song-and-dance to
				# -- get ... injected into the macro; this is done
				# -- by handling of SPREAD_PARAMETRE in Fix.

				if (param == '...') {
					quote(SPREAD_PARAMETRE)
				} else {
					as.symbol(param)
				}

			})) ))
	)

	body(out) <- bmacro( bquote({

		# -- check that the argument list supplied can be
		# --  correctly resolved.
		.( eval(fix_macro_call) )

		MACRO( Must $ Have_Canonical_Arguments() )

		.(
			( as.call(c(
				# -- call the non-variadic form
				fn_sym,
					# -- for each parametre in the (always a closure)
					lapply(
						params,
						function (param) {

							if (param == '...') {
								# -- if the param is ... return `list(...)
								as.call(list(
									as.symbol('list'),
									as.symbol('...') ))
							} else {
								# -- return the param as a symbol
								as.symbol(param)
							}

						}) )) ) )
	}) )

	environment(out) <- env

	out
}
