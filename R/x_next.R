

kiwi_env <- environment()





# is_variadic :: <string> -> <string>
#
# Check if a variable is of the form xMethod_ | x_Method_

is_variadic   <- function (fn_name) {
	grepl('_$', fn_name)
}

# is_unchaining :: <string> -> <string>
#
# Check if a variable is of the form x_Method | x_Method_

is_unchaining <- function (fn_name) {
	grepl('^x_', fn_name)
}






# as_variadic :: <string> -> <string>
#
# Convert a method to the form xMethod_ | x_Method_

as_variadic     <- function (fn_name) {
	if (is_variadic(fn_name)) {
		fn_name
	} else {
		paste0(fn_name, '_')
	}
}

# as_chaining :: <string> -> <string>
#
# Convert a method to the form x_Method | x_Method_

as_chaining   <- function (fn_name) {
	if (!is_unchaining(fn_name)) {
		fn_name
	} else {
		gsub('^x_', 'x', fn_name)
	}
}

# as_nonvariadic :: <string> -> <string>
#
# Convert a method to the form xMethod | x_Method

as_nonvariadic     <- function (fn_name) {
	if (!is_variadic(fn_name)) {
		fn_name
	} else {
		gsub('_$', '', fn_name)
	}
}

# as_unchaining :: <string> -> <string>
#
# Convert a method to the form xMethod | xMethod_

as_unchaining   <- function (fn_name) {
	if (is_unchaining(fn_name)) {
		fn_name
	} else {
		gsub('^x', 'x_', fn_name)
	}
}





# lookup_fn :: <character> -> function
#
# take a method name and find its coresponding function.

lookup_fn <- function (method_name) {

	if (is_unchaining(method_name)) {
		kiwi_env[[ as_chaining(method_name) ]]
	} else {
		kiwi_env[[method_name]]
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

		variadic_fn     <- lookup_fn(fn_name)
		fn              <- kiwi_env[[ as_nonvariadic(fn_name) ]]

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



# has_variadic_param :: <string> -> <logical>
#
# check if a set of parametres contins variadic parametres.

has_variadic_param <- function (params) {
	any(grepl('^[.]{3}', params))
}



# fixed_param :: <string> -> <string> -> <string>
#
#

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

	# create_static_body :: function x <string> x <string> -> Expression
	#
	# create_static_body creates the body for methods in which the
	# LHS only satifies one parametre; that parametre is removed from the function,
	# and in the body Self( ) is used as the argument passed to the underlying function
	# the method is calling to.
	#
	# For variadic functions which fix '...', Self() is given as ..1, and ... is kept
	# as a parametre for additional arguments.

	create_static_body <- function (fn, method_name, fixed) {

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

	# create_dynamic_body
	#
	# (Sorry for using a buzzword)
	#
	# This creates the function body that accomponies methods in which the
	# LHS matches multiple parametres, and the arguments supplied by the user
	# might alter which parametre the LHS is bound to.
	#
	#


	create_dynamic_body <- function (fn, method_name, fixed) {

		# -- this can only be one argument short of
		# -- supplying arguments to its underlying function.

		if (is_unchaining(method_name)) {

			# -- normalise the method name to a function name.
			fn_sym <- as.symbol(as_chaining(method_name))

			bquote({

				argnames <- names(as.list(match.call(expand.dots = False))[-1])
				args <- lapply(argnames, function (param) {
					eval(as.symbol(param))
				})
				names(args) <- argnames

				# -- ensure every argument is supplied (including LHS).
				if (length(args) != length(.( names(formals(fn)) )) - 1) {

					message = "Too few arguments were supplied to the method " %+% .(paste(fn_sym)) %+%
						", as methods\ncannot currently be partially applied.\n"

					throw_kiwi_error(sys.call(), message)
				}

				# -- set the missing argument to the LHS (Self() returns the LHS)
				args[[ setdiff(.( names(formals(fn)) ), names(args)) ]] <-
					quote(Self())

				do.call(.(fn_sym), args)
			})

		} else {

			fn_sym <- as.symbol(method_name)

			# -- chaining methods call x_ on the return value of kiwi function.

			bquote({

				argnames <- names(as.list(match.call(expand.dots = False))[-1])
				args <- lapply(argnames, function (param) {
					eval(as.symbol(param))
				})
				names(args) <- argnames

				# -- ensure every argument is supplied (including LHS).
				if (length(args) != length(.( names(formals(fn)) )) - 1) {

					message = "Too few arguments were supplied to the method " %+% .(paste(fn_sym)) %+%
						", as methods\ncannot currently be partially applied.\n"

					throw_kiwi_error(sys.call(), message)
				}

				# -- set the missing argument to the LHS (Self() returns the LHS)
				args[[ setdiff(.( names(formals(fn)) ), names(args)) ]] <-
					quote(Self())

				x_(do.call(.(fn_sym), args))
			})
		}
	}

	function (fn_name, params) {

		fn                 <- lookup_fn(fn_name)
		fn_sym             <- as.symbol(fn_name)
		fn_params          <- names(formals(fn))
		fn_proto_params    <- as_proto_params(fn_name)

		which_proto_params <- which(fn_proto_params %in%  params)
		which_other_params <- which(fn_proto_params %!in% params)

		method <- function () {}

		formals(method) <- formals(fn)

		if (length(fn_params) == 1 && fn_params == '...') {

			formals(method) <- formals(fn)
			body(method) <- create_static_body(
				fn, fn_name, '...')

		} else if (length(which_proto_params) == 1) {
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

			body(method) <- create_static_body(
				fn, fn_name, fn_params[which_proto_params])

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
			body(method)    <- create_dynamic_body(fn, fn_name)

		}

		environment(method) <- new.env(parent = environment(fn))
		method
	}

})





# make_proto :: <character> -> <character> -> Environment function
#
# make_proto takes kiwi's function names, and a list of parametres
# that flag the function for inclusion in the prototype.

make_proto <- function (fns, params, description) {

	self      <- Object()

	proto_fns <- c(
			fns_with_params(fns, params),
			lapply(
				fns_with_params(fns, params), as_unchaining) )

	for (proto_fn in proto_fns) {
		self[[proto_fn]] <- make_method(proto_fn, params)
	}

	self $ private <- list(contents_are = description)
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






kiwi_fns         <- ls(kiwi_env, pattern = 'x[A-Z]')

x_any_proto      <- make_proto(kiwi_fns, proto_params $ any,        'arbitrary values')
x_table_proto    <- make_proto(kiwi_fns, proto_params $ table,      'data-frames or matrices')
x_factor_proto   <- make_proto(kiwi_fns, proto_params $ factor,     'factors')
x_function_proto <- make_proto(kiwi_fns, proto_params $ `function`, 'functions')
x_coll_proto     <- make_proto(kiwi_fns, proto_params $ coll,       'collections')






# -------------------------------- Type Constructor -------------------------------- #

#' x_
#'
#' Generate an kiwi object with methods available to it.
#'
#' @param
#'    val an arbitrary value. The value to wrap in an
#'    kiwi object.
#'    The methods available depend on the input
#'    type; functions and collections have the most methods available.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An object of class "kiwi". Internally the object is represented as a
#'    list with a single field \bold{x}, but this field cannot be accessed directly.
#'    Instead, the method \bold{$ x_( )} or \bold{$ x_Identity( )} can be used to
#'    return the data stored in an kiwi object.
#'
#'    The methods available to an kiwi object depend on the type of the data it
#'    contains. All kiwi objects inherit a handful of methods regardless of their
#'    type; these include \bold{xIdentity} and \bold{xTap} - a method that allows
#'    anonymous function to be executed on an kiwi object.
#'
#'    The two primary groups of methods are collection methods and function methods.
#'
#'    Matrices, data frames, and factors have methods for converting them to collections,
#'    while normal Kiwi functions are also available as methods for collections
#'    and functions.
#'
#' @section Corner Cases:
#'    The methods that can be used by \bold{$ x_( )} object varies depending
#'    on the type of val.
#'
#' @family methods
#'
#' @example
#'    inst/examples/example-x_.R
#'
#' @rdname x_
#' @export

x_ <- MakeFun('x_', function (val) {
	# Collection any -> Kiwi any
	# type constructor for the method-chaining data type.



	# -- a useful corner case; there are no methods
	# -- specifically for kiwi objects with kiwi
	# -- objects in them. Makes defining methods easier.
	if (any(class(val) == 'kiwi')) {
		val
	} else {
		# -- cannot just be a val with a class label,
		# -- as if val is null then x_ will fail.
		structure(list(x = val), class = 'kiwi')
	}
})

#' @rdname x_
#' @export

x__ <- function (...) {
	x_(list(...))
}







get_proto_ref <- local({

	x_table_members    <- ls(x_table_proto)
	x_factor_members   <- ls(x_factor_proto)
	x_function_members <- ls(x_function_proto)
	x_coll_members     <- ls(x_coll_proto)

	function (val) {
		# get the reference to the appropriate methods.

		# -- keep this code fairly efficient.

		proto_ref <-
		if (is.function( val )) {
			list(x_function_proto, x_function_members)
		} else if (is.matrix( val ) || is.data.frame( val )) {
			list(x_table_proto, x_table_members)
		} else if (is.factor( val )){
			list(x_factor_proto, x_factor_members)
		} else if (is_atomic( val ) || is_generic( val )) {
			list(x_coll_proto, x_coll_members)
		} else {
			stop('one moment')
		}
	}

})






#' @export

`$.kiwi` <- local({

	# some methods are known by their more common
	# but worse names (like filter, filterNot).
	# Meet the user half way and suggest the "proper" name.

	alias <- function (incorrect, correct) {

		forms <- function (fn_name) {

			list(
				fn_name,
				paste0(fn_name, '...'),
				gsub('^x', 'x_', fn_name),
				paste0(
					gsub('^x', 'x_', fn_name),
					 '...')
			)
		}

		structure(
			forms(correct),
			names = forms(incorrect))
	}

	autosuggested <- c(
		alias('x', 'x_'),
		alias('xAsNumeric',   'xAsDouble'),

		alias('xAsChars',     'xToChars'),
		alias('xAsWords',     'xToWords'),
		alias('xAsLines',     'xToLines'),

		alias('xToChars',     'xFromChars'),
		alias('xToWords',     'xFromWords'),
		alias('xToLines',     'xFromLines'),

		alias('xByColkeys',   'xByColrows'),
		alias('xByRowkeys',   'xByRowrows'),
		alias('xAddNames',    'xAddKeys'),

		alias('xC',           'xJoin'),
		alias('xConcat',      'xJoin'),
		alias('xConcatenate', 'xJoin'),

		alias('xFilter',      'xSelect'),
		alias('xFilterNot',   'xReject'),

		alias('xMean',      'xMeanOf'),
		alias('xFilterNot',   'xReject'),

		alias('xGroup',       'xChunk'),
		alias('xZipWith',     'xMapMany'),

		alias('xAll',        'xAllOf'),
		alias('xAny',        'xAnyOf'),
		alias('xArity',      'xArityOf'),
		alias('xDuplicates', 'xDuplicatesOf'),
		alias('xFirst',      'xFirstOf'),
		alias('xFormals',    'xFormalsOf'),
		alias('xFourth',     'xFourthOf'),
		alias('xIndices',    'xIndicesOf'),
		alias('xInit',      'xInitOf'),
		alias('xInter',     'xInterOf'),
		alias('xKeys',      'xKeysOf'),
		alias('xLast',      'xLastOf'),
		alias('xLen',       'xLenOf'),
		alias('xMean',      'xMeanOf'),
		alias('xNone',      'xNoneOf'),
		alias('xOne',       'xOneOf'),
		alias('xOrder',     'xOrderOf'),
		alias('xParams',    'xParamsOf'),
		alias('xPowerSet',  'xPowerSetOf'),
		alias('xProdSet',   'xProdSetOf'),
		alias('xRank',      'xRank'),
		alias('xRest',      'xRestOf'),
		alias('xSecond',    'xSecondOf'),
		alias('xThird',     'xThirdOf'),
		alias('xUnion',     'xUnionOf'),
		alias('xUnique',    'xUniqueOf'),
		alias('xValues',    'xValuesOf')
	)

	suggest_similar_method <- local({
		# -- the spell-checker for Kiwi's methods.

		message <- function (name, contents_are, similar) {

			if (length(similar) == 0) {
				"Could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+% "."
			} else {
				"Could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+%
				":\n" %+%
				colourise$green("did you mean " %+% rsample(similar, size = 1) %+% "?")
			}
		}

		function (val, method_name, contents_are, invoking_call) {
			# given an incorrect method name throw an error
			# suggesting a similar

			proto       <- get_proto_ref(val)
			method_name <- method_name

			candidate_methods <- setdiff(proto[[2]], 'private')

			# -- get the edit distance to each method in the prototype.
			distances <- adist(method_name, candidate_methods)

			similar   <- if ( any(method_name == names(autosuggested)) ) {
				autosuggested[[method_name]]
			} else if (min(distances) < nchar(method_name) / 2) {

				candidate_methods[which.min(distances)]

			} else {
				character(0)
			}

			throw_kiwi_error(
				message = message(method_name, contents_are, similar))
		}
	})

	function (obj, method) {
		# Kiwi a -> symbol -> function
		# return an kiwi method associated with the type a.

		method_name <- paste0(substitute(method))

		proto <- get_proto_ref( obj[['x']] )

		if ( !any(proto[[2]] == method_name) || method_name == "private" ) {
			# -- the invoked method wasn't found, so we should give a suggestion.

			invoking_call <- paste0(' $ ', method_name)

			contents_are <- proto[[1]][['private']][['contents_are']]

			suggest_similar_method(
				obj[['x']], method_name, contents_are, invoking_call)

		}

		fn <- proto[[1]][[method_name]]
		environment(fn)[['Self']] <- function () obj[['x']]

		fn
	}
})

# -------------------------------- Print Method -------------------------------- #

#' @export

print.kiwi <- function (x, ...) {

	proto        <- get_proto_ref( x[['x']] )
	contents_are <- proto[[1]][['private']] [['contents_are']]

	header <- colourise $ blue(
		'\n[ an kiwi object with methods for ' %+% contents_are %+% ' ]')

	cat(
		header  %+% '\n\n' %+%
		'$x_()' %+% '\n')

	print(x $ x_Identity(), ...)
}
