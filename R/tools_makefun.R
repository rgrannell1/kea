
# write_preconditions
#
# returns either null, or a list containing some
# of the fields PRE1, PRE2, and PRE3. These fields will
# have corresponding expressions that check properties of
# the coresponding argument.

write_preconditions <- local({

	param_preconds <- list()

	for (param in c('fn', 'fn1', 'fn2', 'pred', 'pred1', 'pred2')) {
		param_preconds[[param]] <-
			do.call( Must_Be_Fn_Matchable, list(as.symbol(param)) )
	}

	param_preconds $ fns <- {
		Must_Be_Collection(fns)
		Must_Be_Collection_Of_Fn_Matchable(fns)
	}

	param_preconds $ sym <-
		Must_Be_Matchable(substitute(sym))

	for (param in c(
		'coll', 'coll1', 'coll2',
		'bools', 'ims', 'raws',
		'nums', 'num', 'num1', 'num2',
		'str', 'str1', 'str2')) {

		param_preconds[[param]] <-
			do.call( Must_Be_Collection, list(as.symbol(param)) )
	}

	param_preconds $ colls <-
		Must_Be_Collection_Of_Collections(colls)

	function (params) {

		preconds <- list()

		# -- refactor this to make it smaller.

		for (ith in seq_along(params)) {

			key   <- paste0('PRE', ith)
			param <- params[[ith]]

			preconds[[key]] <- param_preconds[[param]]
		}

		if (length(preconds) == 0) {
			list()
		} else {
			preconds
		}
	}
})

# write_boilerplate
#
# this writes code that needs to be executed predictably for
# a given parametre; fn must always be looked-up, for example.
#

write_boilerplate <- function (params) {

	param_boilerplate <- list(
		fn    = list(
			quote(fn <- match_fn(fn))
		),
		pred  = list(
			quote(pred <- match_fn(pred))
		),
		fns   = list(
			quote(lapply(fns, match_fn))
		),
		nums  = list(
			quote(nums <- as_typed_vector(nums, 'numeric'))
		),
		strs  = list(
			quote(strs <- as_typed_vector(strs, 'character'))
		),
		ints  = list(
			quote(ints <- as_typed_vector(ints, 'integer'))
		),
		bools = list(
			quote(bools <- as_typed_vector(bools, 'logical'))
		),
		sym   = list(
			quote(sym   <- substitute(sym)),
			quote(sym   <- paste(sym))
		),

		# str will always be a character vector.
		# it may be a length-one value, or a length-zero value.

		str   = list(
			quote(str <- as_atom(str, "character"))
		),
		str1   = list(
			quote(str1 <- as_atom(str1, "character"))
		),
		str2   = list(
			quote(str2 <- as_atom(str2, "character"))
		)
	)

	expr_body <- list(as.symbol('{'))

	for (ith in seq_along(params)) {

		param <- params[[ith]]

		# -- returns NULL is no corresponding param,
		# -- but this is unit under concatenation.
		boilerplate <- param_boilerplate[[param]]

		expr_body <- c(expr_body, boilerplate)
	}

	if (length(expr_body) == 1) {
		list()
	} else {
		list(FINAL = as.call(expr_body))
	}
}





# TODO split into several, small functions, with MakeFun as a main function.

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

	expr <- substitute(expr)

	# -- evaluate the function, to allow access to its parts.
	fn     <- eval(unquote(expr), parent_frame)
	params <- names(formals(fn))

	# -- the function to ultimately return; all parts will be overwritten.
	boilerplated <- function () {}

	preconds <- write_preconditions(params)
	final    <- write_boilerplate(params)

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

			c(
				# -- the parametres to bind over.
				lapply(params, as.symbol),
				# -- single arguments checks to be injected.
				preconds,
				# -- a final expression to run (coersion functions)
				final
			)
		) ))
	)

	formals(boilerplated) <- formals(fn)
	body(boilerplated)    <- join_exprs(eval(fix_macro_call), body(fn))

	environment(boilerplated) <- parent.frame()

	boilerplated
}











# MakeVariadic
#
# MakeVariadic takes a function, and the variable to fix, and it generates
# a variadic form of a function.

MakeVariadic <- function (fn, fixed) {

	env <- new.env(parent = environment(fn))

	fn_sym    <- as.symbol(substitute(fn))
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

		MACRO( Must_Have_Canonical_Arguments() )

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
