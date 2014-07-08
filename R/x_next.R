
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






make_method <- function (fn, params) {




}








make_proto <- function (fns, params) {

	self      <- Object()
	proto_fns <- c(
			fns_with_params(fns, params),
			lapply(fns_with_params(fns, params), as_unchaining) )

	for (proto_fn in proto_fns) {
		self[[proto_fn]] <- make_method(proto_fn, params)
	}

	self
}






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
