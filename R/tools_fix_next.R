
# -------------------------------- fix -------------------------------- #
#
# fix is an more efficient implementation of xFix, which sets an argument
# of a function permenantly. This will be used by Fix and MakeFun to create
# partially applicable functions.

fix <- local({

	fn_symbol <- as.symbol('.fixed_function')

	function (.fixed_function, coll) {

		fn_formals      <- formals(.fixed_function)
		fn_params       <- names(fn_formals)
		.fixed_function <- substitute(.fixed_function)

		do.call('function', list(
			# -- select each unused parametre.
			as.pairlist(fn_formals[ paste0(seq_along(fn_formals)) %!in% names(coll) ]),
			bquote({
				.(paste0('a partially applied form of ', .fixed_function))

				.( as.call(c(.fixed_function, lapply(seq_along(fn_params), function (ith) {

					# -- the ith parametre has a matching argument.
					if (any( names(coll) == paste0(ith) )) {
						coll[[ paste0(ith) ]]
					} else {
						as.symbol( fn_params[[ith]] )
					}

				}) )) )

			})
		))

	}

})

# -------------------------------- Fix -------------------------------- #
#
# Kea functions are partially applicable,
#
#
#

Fix <- function (FN, SYMS, PRES, FINAL) {

	arity  <- length(SYMS)
	params <- paste(SYMS)

	bquote({

		.fixed_args <- list()

		for (ith in seq_along( .(params) )) {

			param <- .(params)[[ith]]

			if (param == 'SPREAD_PARAMETRE') {
				param <- as.symbol('...')
			} else {
				param <- as.symbol(param)
			}

			if (!do.call( missing, list(param)) ) {

				.fixed_args[[ paste(ith) ]] <- eval(param)
			}

		}

		if ( length(.fixed_args) < length(.(params)) ) {
			return (fix( .(substitute(FN)), .fixed_args ))
		}

		.(substitute(FINAL))

	})

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

	self $ ims   <- quote(ims   <- as_typed_vector(ims,   'complex'))
	self $ ints  <- quote(ints  <- as_typed_vector(ints,  'integer'))
	self $ raws  <- quote(raws  <- as_typed_vector(raws,  'raw'))
	self $ str1  <- quote(str1  <- as_typed_vector(str1,  'character'))
	self $ str2  <- quote(str2  <- as_typed_vector(str2,  'character'))
	self $ bools <- quote(bools <- as_typed_vector(bools, 'logical'))
	self $ rexp  <- quote(rexp  <- as_typed_vector(rexp,  'character'))
	self $ sym   <- quote(sym   <- list(
		quote(sym <- substitute(sym)),
		quote(sym <- paste(sym))
	))
	self $ nums  <- quote(nums <- as_typed_vector(nums, 'numeric'))
	self $ strs  <- quote(strs <- as_typed_vector(strs, 'character'))
	self $ str   <- quote(str  <- as_typed_vector(str,  'character'))
	self $ num   <- quote(num  <- as_typed_vector(num,  'numeric'))
	self $ fns   <- quote(fns  <- lapply(fns, match_fn))
	self $ pred  <- quote(pred <- match_fn(pred))
	self $ fn    <- quote(fn   <- match_fn(fn))

	function (params) {

		final <- list(as.symbol('{'))

		for (ith in seq_along(params)) {
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

MakeFun <- function (sym, expr, typed = True) {

	fn_sym       <- sym
	parent_frame <- parent.frame()

	unquote      <- function (inner) {

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

	# -- get info about the supplied function.
	expr        <- substitute(expr)
	underlying  <- eval(unquote(expr), parent_frame)
	params      <- names(formals(underlying))

	# -- dynamically write the type-checking code

	type_checks      <- write_type_checks(params)
	type_conversions <- write_type_conversions(params)

	# -- this macro calls a second, which creates some boilerplate needed
	# -- to create a partially applied function.

	call_Fix_macro <- bquote(

		.(as.call( list(
			as.symbol('Fix'),
			# -- the Kea function to partially apply.
			as.symbol(fn_sym),

			# -- the parametre to the Kea function.
			lapply(params, as.symbol),
			type_checks,
			type_conversions
		)) )
	)

	decorated <- function () {}

	formals(decorated)     <- formals(underlying)
	body(decorated)        <- join_exprs(eval(call_Fix_macro), body(underlying))
	environment(decorated) <- parent.frame()

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

	if (fixed %!in% params) {
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

	environment(varfn) <- env
	varfn

}
