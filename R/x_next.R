

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

as_proto_params <- function (fn_name) {

	if (is_variadic(fn_name)) {

		variadic_fn <- kiwi_env[[fn_name]]
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
# Within a prototype there is preference for binding to some parametres above
# another (in reality a lot of these won't be found together):
#
#
#
#
#
#
# The rules of parametre fixing are simple enough.
#
# 1, if, for a particular prototype, a parametres
#
# 2, param{n + 1} > param{n}
#
#
#
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

	function (fn_name, params) {

		# -- GT ordering pairs

		fn        <- lookup_fn(fn_name)
		fn_params <- as_proto_params(fn_name)

		print(fn_params)


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
