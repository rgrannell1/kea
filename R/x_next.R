
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
	if (!is_unchaining(fn_name)) {
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

# select all the kiwi functions with at least
# one given parametre, or a particular type of ... parametre.

functions_with_params <- function (params) {

	Filter(
		function (fn_name) {
			any(as_proto_params(fn_name) %in% params)
		},
		ls(kiwi_env, pattern = 'x[A-Z]')
	)
}





simple_method <- list(
	chaining_nonvariadic   = function (fn_sym, fn, fixed) {
		bquote({




		})
	},
	chaining_variadic      = function (fn_sym, fn, fixed) {
		bquote({




		})
	},
	unchaining_nonvariadic = function (fn_sym, fn, fixed) {
		bquote({




		})
	},
	unchaining_variadic    = function (fn_sym, fn, fixed) {
		bquote({




		})
	}
)





proto_params <- list(
	function = c('fn', 'pred', '...fns', '...preds')
)
