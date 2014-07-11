

kiwi_env <- environment()





# Test what form of a function a function name is
#

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
	if (!is_unchaining(fn_name)) {
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


# lookup_fn :: <character> -> function
#
#

lookup_fn <- function (fn_name) {

	if (is_unchaining(fn_name)) {
		kiwi_env[[ as_chaining(fn_name) ]]
	} else {
		kiwi_env[[fn_name]]
	}
}

# as_proto_params :: <character> -> <character>
#
# annotate the ... parametre of a function, if the
# function has one, so that the ... parametre is
# attached to the correct prototype.

as_proto_params <- function (method_name) {

	fn_name <- as_chaining(method_name)

	if (is_variadic(fn_name)) {

		variadic_fn <- lookup_fn(fn_name)
		fn          <- kiwi_env[[ as_nonvariadic(fn_name) ]]

		variadic_params <- names(formals(variadic_fn))
		params          <- names(formals(fn))

		spread_param    <- params[!(params %in% variadic_params)]

		variadic_params[variadic_params == '...'] <- paste0('...', spread_param)
		variadic_params

	} else {

		fn <- kiwi_env[[fn_name]]
		names(formals(fn))

	}
}





has_variadic_param <- function (params) {
	any(grepl('^[.]{3}', params))
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

# make_method :: <character> -> <character> -> function
#
#
#
# make method generates a method from a kiwi function.
#
# make_method solves some problems with first-generation kiwi-methods;
# each form of the method would have to explicitely added by hand, and
# methods that should be available in multiple forms couldn't be.
#
# the new methods should allow the LHS argument to default to a particular
# parametre based on a prototye, but by calling the method with a named argument
# the method should intelligently decide which parametre to bind to the LHS.
#
#     x_(str1) $ xWrite(str2)
#
# or
#
#     x_(str2) $ xWrite(str1 = str1)
#
#
# Within a prototype there is preference for binding to some parametres above
# another (in reality a lot of these won't be found together):
#
#
#
#
#
#
#                        THE RULES
#
# 1, every method is available in the method chain.
#
# 2, every method has the same parametres as the function.
#
# 3, if too many arguments are given, an error is thrown saying the
#    LHS couldn't be bound to any parametre.
#
# 4, if too few arguments are given, an error is thrown before
#    kiwi's partial application kicks in. This prevents ambiguities with
#    rebinding Self() after the user fixes another parametre.
#
# 5, The LHS will preferentially bind to one of the methods parameters;
#    for example collections will bind to 'coll' more preferably than
#    to the parametre 'fn'.
#
#    In this way, a call to a method without using names should never be
#    ambigious.
#
# 6, By calling a method with named arguments you can choose a parametre
#    to supply an argument to, as is normal for functions. In this case
#    the LHS is bound to it's second favourite parametre.
#
# 7, If the LHS is bound to a parametre for which it couldn't possibly
#    be correct (a collection to 'fn') an error is thrown.
#



make_method <- local({

	param_preceedence <- function (param1, param2) {

		# -- a partial relation; declaring an exhaustive
		# -- relation would be a complete pain.
		#
		# -- if an odd combination of parametres
		# -- is used this must be updated.

		match <- Filter(
			function (pair) {
				all(c(param1, param2) %in% pair)
			},
			list(
				c('coll2', 'coll1'),
				c('str2', 'str1')
			)
		)

		if (length(match) == 0) {
			stop('internal error! no match found for ordering of ', param1, ' ', param2)
		}

	}

	# create_unambigious_body :: function x <string> x <string> -> Expression
	#
	# create_unambigious_body creates the body for methods in which the
	# LHS only satifies one parametre; that parametre is removed from the function,
	# and in the body Self( ) is used as the argument passed to the underlying function
	# the method is calling to.
	#
	# For variadic functions which fix '...', Self() is given as ..1, and ... is kept
	# as a parametre for additional arguments.

	create_unambigious_body <- function (fn, method_name, fixed) {

		# -- accumulate a parametres list.
		# -- done with Reduce as more work is needed for variadic formals.
		arglist <- Reduce(
			function (acc, param) {

				if (param == fixed) {
					# -- this parametre is to be fixed.

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

		if (is_unchaining(method_name)) {

			# -- normalise the method name to a function name.
			fn_sym <- as.symbol(as_chaining(method_name))

			bquote({
				.(( as.call(c(fn_sym, arglist)) ))
			})

		} else {

			fn_sym <- as.symbol(method_name)

			# -- chaining methods call x_ on the return value of kiwi function.
			bquote({
				x_( .(( as.call(c(fn_sym, arglist)) )) )
			})
		}

	}


	function (fn_name, params) {

		fn                 <- lookup_fn(fn_name)
		fn_sym             <- as.symbol(fn_name)
		fn_params          <- names(formals(fn))
		fn_proto_params    <- as_proto_params(fn_name)

		which_proto_params <- which(fn_proto_params %in% params)
		which_other_params <- which(fn_proto_params %!in% params)

		method <- function () {}

		formals(method) <- formals(fn)

		if (length(which_proto_params) == 1) {
			# -- the LHS only satifies one parametre.
			# -- so that parametre cannot be set by the user.
			# -- remove the parametre from the method's formals.

			param_is_variadic <-
				has_variadic_param(fn_proto_params[which_other_params])

			formals(method) <- if (!param_is_variadic) {
				formals(fn)[which_other_params]
			} else {
				# -- variadic parametres can take multiple arguments;
				# -- set the LHS to ...1, and keep ... around to take more args.
				formals(fn)
			}

			body(method) <- create_unambigious_body(
				fn, fn_name, fn_params[which_proto_params])

			print(body(method))

		} else {
			# -- the LHS satisfies multiple parametres, so
			# -- the user may possibly choose which parametre
			# -- it gets bound to.
			#
			# -- in this case, a parametre cannot be removed from
			# -- the method, and the parametre that the LHS
			# -- gets bound to is determined at runtime by seeing what
			# -- parametres are free after the user supplied (possibly named)
			# -- input.

			formals(method) <- formals(fn)
		}

		if (fn_name == 'xAllOf' && 'coll' %in% params) {

			print(fn_proto_params)

			print(params)
		}

		body(method) <- bquote({

			# -- this can only be one argument short of
			# -- supplying arguments to its underlying function.
			args <- as.list(match.call())[-1]

			# -- set the LHS to the highest-preference
			# -- unnamed argument.

			print(args)

			x_( .(as.call(list(fn_sym))) )

		})

		environment(method) <- new.env(parent = environment(fn))
		method
	}

})





# make_proto :: <character> -> <character> -> Environment function
#
# make_proto takes kiwi's function names, and a list of parametres
# that flag the function for inclusion in the prototype.

make_proto <- function (fns, params) {

	self      <- Object()

	proto_fns <- c(
			fns_with_params(fns, params),
			lapply(
				fns_with_params(fns, params), as_unchaining) )

	for (proto_fn in proto_fns) {
		self[[proto_fn]] <- make_method(proto_fn, params)
	}

	self
}




# proto_params
#
# A list with an element for each prototype. The parametres are unordered here;
# if a function has multiple parametres that belong in one prototype a relation
# determines their preceedence within make_method.

proto_params <- list(
	table      = c('tab'),
	factor     = c('fact'),

	any        = c('val', 'val1', 'val2'),
	`function` = c('fn', 'pred', '...fns', '...preds', 'val', 'val1', 'val2'),
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
		'colls', '...colls',
		'val', 'val1', 'val2'
	)
)






kiwi_fns <- ls(kiwi_env, pattern = 'x[A-Z]')

kiwi_table_proto    <- make_proto(kiwi_fns, proto_params $ table)
kiwi_factor_proto   <- make_proto(kiwi_fns, proto_params $ factor)
kiwi_function_proto <- make_proto(kiwi_fns, proto_params $ `function`)
kiwi_coll_proto     <- make_proto(kiwi_fns, proto_params $ coll)
