
# Test what form of a function a function name is
#

kiwi_env <- environment()






is_variadic   <- function (fn_name) {
	grepl('_$', fn_name)
}

is_unchaining <- function (fn_name) {
	grepl('^x_', fn_name)
}





# Convert a function name to another form
#

as_variadic     <- function (fn_name) {
	if (is_variadic(fn_name)) {
		fn_name
	} else {
		paste0(fn_name, '_')
	}
}

as_chaining   <- function (fn_name) {
	if (is_unchaining(fn_name)) {
		fn_name
	} else {
		gsub('^x_', 'x', fn_name)
	}
}

as_nonvariadic     <- function (fn_name) {
	if (!is_variadic(fn_name)) {
		fn_name
	} else {
		gsub('_$', '', fn_name)
	}
}

as_unchaining   <- function (fn_name) {
	if (is_unchaining(fn_name)) {
		fn_name
	} else {
		gsub('^x', 'x_', fn_name)
	}
}




# annotate the ... parametre of a function, if the
# function has one, so that the ... parametre is
# attached to the correct prototype.

as_proto_params <- function (fn_name) {

	if (is_variadic(fn_name)) {

		variadic_fn <- kiwi_env[[fn_name]]
		fn          <- kiwi_env[[ as_nonvariadic(fn_name) ]]

		variadic_params <- names(formals(variadic_fn))
		params <- names(formals(fn))

		spread_param <- params[!(params %in% variadic_params)]

		variadic_params[variadic_params == '...'] <- paste0('...', spread_param)
		variadic_params

	} else {

		fn <- kiwi_env[[fn_name]]
		names(formals(fn))

	}
}





fixed_param <- function (fn_name, params) {

	fn_params <- as_proto_params(fn_name)

	fn_params[ which(fn_params %in% params)[[1]] ]


}






# select all the kiwi functions with at least
# one given parametre, or a particular type of ... parametre.

fns_with_params <- function (fns, params) {

	Filter(
		function (fn_name) {
			any(as_proto_params(fn_name) %in% params)
		},
		fns
	)
}








make_proto <- function (fns, params) {

	self      <- Object()
	proto_fns <- c(
			fns_with_params(fns, params),
			lapply(fns_with_params(fns, params), as_unchaining) )

	for (proto_fn in proto_fns) {

		fn <- kiwi_env[[proto_fn]]

		if (is_variadic(proto_fn)) {
			if (is_unchaining(proto_fn)) {

				self[[proto_fn]] <- simple_method $ unchaining_variadic(
					as.symbol(proto_fn), fn, fixed_param(proto_fn, params))

			} else {

				self[[proto_fn]] <- simple_method $ chaining_variadic(
					as.symbol(proto_fn), fn, fixed_param(proto_fn, params))

			}
		} else if (is_unchaining(proto_fn)) {

			self[[proto_fn]] <- simple_method $ unchaining_nonvariadic(
				as.symbol(proto_fn), fn, fixed_param(proto_fn, params))

		} else {

			self[[proto_fn]] <- simple_method $ chaining_nonvariadic(
				as.symbol(proto_fn), fn, fixed_param(proto_fn, params))

		}


	}

	self
}






# simple_method
#
# Many functions can only exist in one form within
# one prototype. For these functions the method body can be
# simplified.

simple_method <- list(
	chaining_nonvariadic = function (fn_sym, fn, fixed) {
		bquote({

			x_(.(
				as.call( c(fn_sym, lapply(
					names(formals(fn)), function (param) {

						if (as.symbol(param) == fixed) {
							# -- set this argument to the LHS
							quote(Self())
						} else {
							# -- wait for supplied argument, keep parametre.
							as.symbol(param)
						}
					}
				)) )
			))

		})
	},
	chaining_variadic = function (fn_sym, fn, fixed) {

		params <- Reduce(
			function (acc, param) {

				# -- this parametre is to be fixed.

				if (param == fixed) {

					if (fixed == '...') {
						# -- fixing an ellipsis parametre
						c( acc, quote(Self()), as.symbol('...') )
					} else {
						# -- normal fixing
						c( acc, quote(Self()) )
					}

				} else {
					# -- don't fix this parametre.
					c(acc, as.symbol(param))
				}
			},
			names(formals(fn)),
			list()
		)

		bquote({
			x_(.( as.call(c(fn_sym, params)) ))
		})
	},
	unchaining_nonvariadic = function (fn_sym, fn, fixed) {
		bquote({

			.(
				as.call( c(fn_sym, lapply(
					names(formals(fn)), function (param) {

						if (as.symbol(param) == fixed) {
							# -- set this argument to the LHS
							quote(Self())
						} else {
							# -- wait for supplied argument, keep parametre.
							as.symbol(param)
						}
					}
				)) )
			)

		})
	},
	unchaining_variadic = function (fn_sym, fn, fixed) {

		params <- Reduce(
			function (acc, param) {

				# -- this parametre is to be fixed.

				if (param == fixed) {

					if (fixed == '...') {
						# -- fixing an ellipsis parametre
						c( acc, quote(Self()), as.symbol('...') )
					} else {
						# -- normal fixing
						c( acc, quote(Self()) )
					}

				} else {
					# -- don't fix this parametre.
					c(acc, as.symbol(param))
				}
			},
			names(formals(fn)),
			list()
		)

		bquote({
			.( as.call(c(fn_sym, params)) )
		})
	}
)







bquote({

	.call <- sys.call()


})








proto_params <- list(
	table      = c('tab'),
	factor     = c('fact'),

	any        = c('val', 'val1', 'val2'),
	`function` = c('fn', 'pred', '...fns', '...preds'),
	coll       = c(
		'ims',   '...ims',
		'ints',  '...ints',
		'raws',  '...raws',
		'str1',
		'str2',
		'bools', '...bools',
		'rexp',
		'nums',  '...nums',
		'strs',  '...strs',
		'str',
		'num',
		'coll1',  '...coll1',
		'coll2',  '...coll2',
		'fns',
		'preds',
		'coll',  '...coll',
		'colls', '...colls'
	)
)





kiwi_fns <- ls(kiwi_env, pattern = 'x[A-Z]')

kiwi_table_proto    <- make_proto(kiwi_fns, proto_params $ table)
kiwi_factor_proto   <- make_proto(kiwi_fns, proto_params $ factor)
kiwi_any_proto      <- make_proto(kiwi_fns, proto_params $ any)
kiwi_function_proto <- make_proto(kiwi_fns, proto_params $ `function`)
kiwi_coll_proto     <- make_proto(kiwi_fns, proto_params $ coll)
