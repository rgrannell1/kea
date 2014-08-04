
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
	self $ rexp  <- quote(rexp  <- as_typed_vector(rexp,  'logical'))
	self $ sym   <- quote(sym   <- list(
		quote(sym <- substitute(sym)),
		quote(sym <- paste(sym))
	))
	self $ nums  <- quote(nums <- as_typed_vector(nums, 'numeric'))
	self $ strs  <- quote(strs <- as_typed_vector(strs, 'character'))
	self $ str   <- quote(str  <- as_typed_vector(str, 'character'))
	self $ num   <- quote(num  <- as_typed_vector(num, 'numeric'))
	self $ fns   <- quote(fns  <- lapply(fns, match_fn))
	self $ pred  <- quote(pred <- match_pred(pred))
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
