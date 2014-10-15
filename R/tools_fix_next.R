
# -------------------------------- fix -------------------------------- #
#
# fix is an more efficient implementation of xFix, which sets an argument
# of a function permenantly. This will be used by Fix and MakeFun to create
# partially applicable functions.

fix <- function (fixed_function, coll) {

	fn_formals  <- formals(fixed_function)
	fn_params   <- names(fn_formals)

	fn_sym      <- substitute(fixed_function)

	lower_arity <- do.call('function', list(
		# -- select each unused parametre.

		as.pairlist(fn_formals[ fn_params %not_in% names(coll) ]),
		bquote({
			.(paste0('a partially applied form of ', fn_sym))

			.( as.call(c(fn_sym, lapply(fn_params, function (param) {

				# -- the ith parametre has a matching argument.

				if (any( names(coll) == param )) {
					coll[[param]]
				} else {
					as.symbol(param)
				}

			}) )) )

		})
	))

	# copy the environment from the parent.
	environment(lower_arity) <- new.env(parent = environment(fixed_function))
	lower_arity
}

# -------------------------------- Fix -------------------------------- #
#
# Kea functions are partially applicable,
#
#
#

Fix <- function (FN, SYMS, PRES, FINAL) {

	arity  <- length(SYMS)
	params <- paste(SYMS)

	is_variadic <- 'SPREAD_PARAMETRE' %in% params

	preconditions <- Reduce(join_exprs, PRES)

	named_indices        <- seq_len(length(params))
	names(named_indices) <- params





	fix_expr <- if (arity == 1 && !is_variadic) {
		# -- unary non-variadic functions.

		bquote({

			# THE EXCLUSION OF BRACES IS DELIBERATE (efficiency).
			if (missing( .(as.symbol(params)) ))
				return ( .(substitute(FN)) )


		})

	} else if (arity == 1 && is_variadic) {
		# -- unary variadic functions.

		list()

	} else if (!is_variadic) {
		# -- the much more complicated, slower general case.
		# -- efficiency subcases should be found where possible.

		missing_check <- if (length(params) == 1) {
			# -- ever so slightly faster (no function call to c)

			bquote(missing( .( as.symbol(params) ) ))

		} else {
			# -- vapply and lapply are no better right now.

			as.call(c( c, lapply(named_indices, function (ith) {

				bquote(missing( .(as.symbol( params[[ith]] )) ))

			}) ))

		}





		bquote({

			if (nargs() == 0L) {
				# -- fast track for a call with no arguments and NO POSITIONAL EMPTY ARGUMENTS.
				return ( .(substitute(FN)) )
			}

			# -- filter out arguments that were positionally matched, but empty.
			# -- ~80% as slow as the previous for-loop approach.

			is_missing <- .(missing_check)

			if (any(is_missing)) {
				# -- the fix macro must be called.

				# -- get the arguments.
				# -- expand.dots not needed (not dot arguments).
				params <- names(is_missing[!is_missing])
				frame  <- environment()

				# THE EXCLUSION OF BRACES IS VERY DELIBERATE.
				# each use of braces is a function call, and this is a very tight inner-loop.
				args   <- lapply(params, function (param)

					if (param == 'sym')
						substitute(param)
					 else
						frame[[param]]

				)

				# THE EXCLUSION OF BRACES IS VERY DELIBERATE.
				if (length(args) == 0)
					# -- return the function, unchanged.
					# -- will work for missing arguments (unlike fast track) since args filters out missing values.

					return (.(substitute(FN)))

				else if ( length(args) != .(arity) )
					# -- return the function with some arguments fixed.

					names(args) <- params
					return (fix(.(substitute(FN)), args))

			}
			# -- else fast-track non-partial application.
		})

	} else if (is_variadic) {

		# DEDUPLICATE
		# DEDUPLICATE
		# DEDUPLICATE
		# DEDUPLICATE
		# DEDUPLICATE
		# DEDUPLICATE

		missing_check <- if (length(params) == 1) {
			# -- ever so slightly faster (no function call to c)

			bquote(missing( .( as.symbol(params) ) ))

		} else {
			# -- vapply and lapply are no better right now.

			as.call(c( c, lapply(named_indices, function (ith) {

				param <- params[[ith]]

				if (param == 'SPREAD_PARAMETRE') {
					bquote(missing( .( as.symbol('...') ) ))
				} else {
					bquote(missing( .( as.symbol(param) ) ))
				}

			}) ))

		}





		bquote({

			if (nargs() == 0L) {
				# -- fast track for a call with no arguments and NO POSITIONAL EMPTY ARGUMENTS.
				return ( .(substitute(FN)) )
			}

			# -- filter out arguments that were positionally matched, but empty.
			# -- ~80% as slow as the previous for-loop approach.

			is_missing <- .(missing_check)

			if (any(is_missing)) {
				# -- the fix macro must be called.

				# -- get the arguments.
				# -- expand.dots not needed (not dot arguments).
				params <- names(is_missing[!is_missing])
				frame  <- environment()

				# THE EXCLUSION OF BRACES IS VERY DELIBERATE.
				# each use of braces is a function call, and this is a very tight inner-loop.
				args   <- lapply(params, function (param)

					if (param == 'sym')
						substitute(param)
					 else if (param == '...')
					 	list(...)
					 else
						frame[[param]]

				)

				# THE EXCLUSION OF BRACES IS VERY DELIBERATE.
				if (length(args) == 0)
					# -- return the function, unchanged.
					# -- will work for missing arguments (unlike fast track) since args filters out missing values.

					return (.(substitute(FN)))

				else if ( length(args) != .(arity) )
					# -- return the function with some arguments fixed.

					names(args) <- params
					return (fix(.(substitute(FN)), args))

			}
			# -- else fast-track non-partial application.
		})
	}












	# -- TODO: check each precondition upon recieving argument.
	Reduce(join_exprs, c(fix_expr, preconditions, substitute(FINAL)))

}





# -------------------------------- write_type_checks -------------------------------- #
#
# Kea does type-checking on its input arguments; this
# module creates that code.

write_type_checks <- ( function () {

	self <- list()

	for (param in c('fn', 'fn1', 'fn2', 'pred', 'pred1', 'pred2')) {
		self[[param]] <-
			do.call( Must_Be_Fn_Matchable, list(as.symbol(param)) )
	}

	# -- fns must be both a collection and a collection of functions.
	self $ fns <- join_exprs(
		Must_Be_Collection(fns),
		Must_Be_Collection_Of_Fn_Matchable(fns)
	)

	# -- sym must always be a matchable name data-type.
	self $ sym <-
		Must_Be_Matchable(substitute(sym))

	# -- these parametres are always collections.
	for (param in c(
		'coll', 'coll1', 'coll2', 'bools', 'ims', 'raws', 'nums')) {

		self[[param]] <-
			do.call( Must_Be_Collection, list(as.symbol(param)) )
	}

	self $ num <- join_exprs(
		join_exprs(
			Must_Be_Collection(num),
			Must_Be_Non_Nan(num)
		),
		Must_Be_Non_Na(num)
	)

	self $ rexp <- join_exprs(
		join_exprs(
			Must_Be_Collection(rexp),
			Must_Be_Non_Na(rexp)
		),
		quote( check_regexp(rexp, sys.call()) )
	)

	self $ colls <- join_exprs(
		Must_Be_Collection(colls),
		Must_Be_Collection_Of_Collections(colls)
	)

	self $ str <- join_exprs(
		Must_Be_Collection(str),
		Must_Be_Non_Na(str)
	)

	self $ str1 <- join_exprs(
		Must_Be_Collection(str1),
		Must_Be_Non_Na(str1)
	)

	self $ str2 <- join_exprs(
		Must_Be_Collection(str2),
		Must_Be_Non_Na(str2)
	)

	self $ strs <-
		Must_Be_Collection(strs)

	function (params) {

		unname( lapply(params, function (param) {
			self[[param]]
		}) )

	}

} )()



# -------------------------------- write_type_conversions -------------------------------- #
#
# Kea alters some parametres after checking it's type; this module
# writes that code.

write_type_conversions <- ( function () {

	self <- list()

	self $ ims   <- list( quote(ims   <- as_typed_vector(ims,   'complex')) )
	self $ ints  <- list( quote(ints  <- as_typed_vector(ints,  'integer')) )
	self $ raws  <- list( quote(raws  <- as_typed_vector(raws,  'raw')) )
	self $ str1  <- list( quote(str1  <- as_atom(str1,  'character')) )
	self $ str2  <- list( quote(str2  <- as_atom(str2,  'character')) )
	self $ bools <- list( quote(bools <- as_typed_vector(bools, 'logical')) )
	self $ rexp  <- list( quote(rexp  <- as_atom(rexp,  'character')) )
	self $ sym   <- list(
		quote(sym <- substitute(sym)),
		quote(sym <- paste(sym))
	)
	self $ nums  <- list( quote(nums <- as_typed_vector(nums, 'numeric')) )
	self $ strs  <- list( quote(strs <- as_typed_vector(strs, 'character')) )
	self $ str   <- list( quote(str  <- as_atom(str,  'character')) )
	self $ num   <- list( quote(num  <- as_atom(num,  'numeric')) )
	self $ fns   <- list( quote(fns  <- lapply(fns, match_fn)) )
	self $ pred  <- list( quote(pred <- match_fn(pred)) )
	self $ fn    <- list( quote(fn   <- match_fn(fn)) )

	function (params) {

		final <- list(as.symbol('{'))

		for ( ith in seq_len(length(params)) ) {
			final <- c(final, self[[ params[ith] ]])
		}

		if (length(final) == 1) {
			list()
		} else {
			as.call(final)
		}

	}

} )()










# -------------------------------- MakeFun -------------------------------- #
#
# MakeFun takes a function name like xMap, and
# a function definition. It returns a partially-applicable
# function with type-checks automatically written into it.

MakeFun <- function (expr, typed = True, env = parent.frame()) {

	unquote <- function (inner) {

		if (is.pairlist(inner)) {
			as.pairlist(lapply(inner, unquote))
		} else if (length(inner) <= 1L) {
			inner
		} else if (inner[[1L]] == as.name("MACRO")) {
			eval(inner[[2L]], env)
		} else {
			as.call(lapply(inner, unquote))
		}
	}

	# -- get info about the supplied function.
	expr       <- substitute(expr)
	underlying <- eval(unquote(expr), env)
	params     <- names(formals(underlying))

	# -- dynamically write the type-checking code

	type_checks      <- write_type_checks(params)
	type_conversions <- write_type_conversions(params)

	# -- this macro calls a second, which creates some boilerplate needed
	# -- to create a partially applied function.

	call_Fix_macro <- bquote(

		.(as.call( list(
			as.symbol('Fix'),
			# -- the Kea function to partially apply.
			quote(fn_sym),

			# -- the parametre to the Kea function.
			lapply(params, as.symbol),
			type_checks,
			type_conversions
		)) )
	)

	decorated <- function () {}

	formals(decorated) <- formals(underlying)
	body(decorated)    <- join_exprs(eval(call_Fix_macro), body(underlying))

	# allow the function to refer to its original self, pre-partial application!
	#

	environment(decorated)          <- new.env(parent = env)
	environment(decorated) $ fn_sym <- decorated

	decorated
}










# -------------------------------- MakeVariadic -------------------------------- #
#
# MakeVariadic is not a general function; it takes a Kea function, and makes one
# of its arguments variadic.

MakeVariadic <- function (fn, fixed) {

	env <- new.env(parent = environment(fn))

	fn_sym    <- as.symbol(substitute(fn))
	varfn_sym <- as.symbol(paste0(fn_sym, '_'))

	if ( grepl('_', paste0(fn_sym)) ) {
		stop("MakeVariadic: _ in method name ", paste0(fn_sym))
	}

	# -- will replace formals & body, env will be same.

	varfn <- fn

	# -- will break if defaults are ever added to kea.

	params <- names(formals(fn))

	if (fixed != params[length(params)]) {
		stop("MakeVariadic: last param isn't fixed in ", fn_sym)
	}

	if (fixed %not_in% params) {
		stop("MakeVariadic: tried to fix param that doesn't exist ", paste0(fn_sym))
	}

	params[params == fixed] <- '...'

	# -- create a formal list from the new parametres with no defaults.
	formals(varfn) <- as_formals(params)

	call_Fix_macro <- bquote(

		.(as.call( list(
			as.symbol('Fix'),
			# -- the Kea function to partially apply.
			as.symbol(fn_sym),

			# -- the parametre to the Kea function.
			lapply(params, function (param) {
				# -- needed to go through a song-and-dance to
				# -- get ... injected into the macro; this is done
				# -- by handling of SPREAD_PARAMETRE in Fix.

				if (param == '...') {
					quote(SPREAD_PARAMETRE)
				} else {
					as.symbol(param)
				}

			}),
			list(),
			list()
		)) )
	)

	body(varfn) <- bmacro( bquote({

		# -- check that the argument list supplied can be
		# --  correctly resolved.
		.( eval(call_Fix_macro) )

		.(

			# -- this issue only arises for non-unary variadic functions.
			if (length(params) == 1 && params == '...') {
				quote( MACRO( Must_Have_Canonical_Arguments() ) )
			} else {
				NULL
			}

		)

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

	environment(varfn) <- env
	varfn

}
