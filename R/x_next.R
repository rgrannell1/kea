

kea_env <- environment()





# is_variadic :: <string> -> <string>
#
# Check if a variable is of the form xMethod_ | x_Method_

is_variadic <- function (fn_name) {
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

as_variadic <- function (fn_name) {
	if (is_variadic(fn_name)) {
		fn_name
	} else {
		paste0(fn_name, '_')
	}
}

# as_chaining :: <string> -> <string>
#
# Convert a method to the form x_Method | x_Method_

as_chaining <- function (fn_name) {
	if (!is_unchaining(fn_name)) {
		fn_name
	} else {
		gsub('^x_', 'x', fn_name)
	}
}

# as_nonvariadic :: <string> -> <string>
#
# Convert a method to the form xMethod | x_Method

as_nonvariadic <- function (fn_name) {
	if (!is_variadic(fn_name)) {
		fn_name
	} else {
		gsub('_$', '', fn_name)
	}
}

# as_unchaining :: <string> -> <string>
#
# Convert a method to the form xMethod | xMethod_

as_unchaining <- function (fn_name) {
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
		kea_env[[ as_chaining(method_name) ]]
	} else {
		kea_env[[method_name]]
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
		fn              <- kea_env[[ as_nonvariadic(fn_name) ]]

		variadic_params <- names(formals(variadic_fn))
		params          <- names(formals(fn))

		spread_param    <- params[!(params %is_in% variadic_params)]

		variadic_params[variadic_params == '...'] <- paste0('...', spread_param)
		variadic_params

	} else {

		fn <- kea_env[[fn_name]]
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
	fn_params[ which(fn_params %is_in% params)[[1]] ]
}






# select all the kea functions with at least
# one given parametre, or a particular type of ... parametre.

fns_with_params <- function (fns, params) {

	Filter(
		function (fn_name) {
			any(as_proto_params(fn_name) %is_in% params)
		},
		fns
	)
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
	arglist <- Reduce(
		function (acc, param) {

			if (param == fixed) {

				if (fixed == '...') {
					# -- fixing an ellipsis parametre, which
					# -- leaves the ellipsis parametre open for more arguments.
					# -- the function sub_self binds any occurence of 'self' in the supplied argument
					# -- to the value of Self()
					c( acc, quote(Self()), bquote(sub_self( alist( .(as.symbol('...')) ) )) )
				} else {
					# -- normal fixing
					c( acc, quote(Self()) )
				}

			} else {
				# -- don't fix this parametre.
				# -- the function sub_self binds any occurence of 'self' in the supplied argument
				# -- to the value of Self()
				c( acc, bquote(sub_self( .(as.symbol(param)) )) )
			}
		},
		names(formals(fn)),
		list()
	)

	fn_sym <- if (is_unchaining(method_name)) {
		as.symbol(as_chaining(method_name))
	} else {
		as.symbol(method_name)
	}

	# -- normalise the method name to a function name.

	bquote({

		parent_frame     <- parent.frame()
		clone_env        <- new.env(parent = parent_frame)
		clone_env $ self <- Self()

		# -- the value of Self( ) is set when calling the method with $,
		# -- so this function must be supplied in the method body to close over 'Self( )'.
		sub_self <- function (val) {

			eval(substitute_q(
				substitute_q(
					substitute(val), parent_frame), clone_env))

		}

		.({

			method_call <- as.call(c(fn_sym, arglist))

			if (is_unchaining(method_name)) method_call else call('x_', method_call)

		})

	})

}










# create_dynamic_body
#
# (Sorry for using a buzzword)
#
# This creates the function body that accomponies methods in which the
# LHS matches multiple parametres, and the arguments supplied by the user
# might alter which parametre the LHS is bound to.

create_dynamic_body <- function (fn, method_name) {

	# -- this can only be one argument short of
	# -- supplying arguments to its underlying function.


	fn_sym <- if (is_unchaining(method_name)) {
		as.symbol(as_chaining(method_name))
	} else {
		as.symbol(method_name)
	}

	bquote({

		# to allow for self references the parametre must be
		# 'looked-up' in a special environment with self defined.

		invoking_call    <- match.call(definition = sys.function(), call = sys.call() )

		clone_env        <- new.env(parent = parent.frame())
		clone_env $ self <- Self()

		argnames <- names(as.list(match.call(expand.dots = True))[-1])

		args <- lapply(argnames, as.null)

		for (ith in seq_along(argnames)) {

			param <- as.symbol( argnames[[ith]] )

			args[[ith]] <- eval(eval(
				call( 'substitute', eval(call('substitute', param)), clone_env ), clone_env
			), clone_env)

		}

		names(args) <- argnames

		# -- ensure every argument is supplied (including LHS).
		if (length(args) != length(.( names(formals(fn)) )) - 1) {

			message = "Too few arguments were supplied to the method " %+% .(paste(fn_sym)) %+%
				", as methods\ncannot currently be partially applied.\n"

			throw_kea_error(sys.call(), message)
		}

		# -- set the missing argument to the LHS (Self() returns the LHS)
		args[[ setdiff(.( names(formals(fn)) ), names(args)) ]] <- quote(Self())

		# -- reorder the arguments so that with their names removed they
		# -- positionally match in the same way they would by name.

		unnamed_args <- lapply( .( names(formals(fn)) ), function (param) {
			args[[param]]
		})

		# -- unname
		.(

			if (is_unchaining(method_name)) {
				call('do.call', fn_sym, quote(unnamed_args))
			} else {
				call( 'x_', call('do.call', fn_sym, quote(unnamed_args)) )
			}

		)

	})
}









# make_method :: <character> -> <character> -> function
#
# make method generates a method from a kea function.
#
# make_method solves some problems with first-generation kea-methods;
# each form of the method would have to explicitely added by hand, and
# methods that should be available in multiple forms werent.
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
##
# 3, if too many arguments are given, an error is thrown saying the
#    LHS couldn't be bound to any parametre.
#
# 4, if too few arguments are given, an error is thrown before
#    kea's partial application kicks in. This prevents ambiguities with
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

make_method <- function (fn_name, params) {

	fn                 <- lookup_fn(fn_name)
	fn_sym             <- as.symbol(fn_name)
	fn_params          <- names(formals(fn))

	# -- expand variadic parametre (...) to its replacement ...coll, ...fns.
	fn_proto_params    <- as_proto_params(fn_name)

	# -- which parametres (including the typed variadic parametre) are in this prototype?
	which_proto_params <- which(fn_proto_params %is_in%  params)
	# -- which aren't?
	which_other_params <- which(fn_proto_params %not_in% params)

	method <- function () {}

	formals(method) <- formals(fn)

	if (length(fn_params) == 1 && fn_params == '...') {
		# -- the function just has a single variadic parametre.

		formals(method) <- formals(fn)
		body(method)    <- create_static_body(fn, fn_name, '...')

	} else if (length(which_proto_params) == 1) {
		# -- the LHS only satifies one parametre; only
		# -- one param is in the prototype.
		# -- so that parametre cannot be set by the user.
		# -- remove the parametre from the method's formals.

		# -- is the sole parametre being fixed as self() variadic?
		param_is_variadic <- has_variadic_param(fn_proto_params[which_proto_params])
		formals(method)   <- if (param_is_variadic) {
			# -- variadic parametres can take multiple arguments;
			# -- set the LHS to ...1, and keep ... around to take more args.
			formals(fn)
		} else {
			formals(fn)[which_other_params]
		}

		body(method) <- create_static_body(fn, fn_name, fn_params[which_proto_params])

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





# make_proto :: <character> -> <character> -> Environment function
#
# make_proto takes kea's function names, and a list of parametres
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
	table      = c('tab',  'val', 'val1', 'val2',  '...coll', '...coll1', '...coll2'),
	factor     = c('fact', 'val', 'val1', 'val2',  '...coll', '...coll1', '...coll2'),

	any        = c('val', 'val1', 'val2',  '...coll', '...coll1', '...coll2'),
	`function` = c('fn', 'pred', '...fns', '...preds', 'val', 'val1', 'val2', '...coll', '...coll1', '...coll2'),
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






kea_fns         <- ls(kea_env, pattern = 'x[A-Z]')

x_any_proto      <- make_proto(kea_fns, proto_params $ any,        'arbitrary values')
x_table_proto    <- make_proto(kea_fns, proto_params $ table,      'data-frames or matrices')
x_factor_proto   <- make_proto(kea_fns, proto_params $ factor,     'factors')
x_function_proto <- make_proto(kea_fns, proto_params $ `function`, 'functions')
x_coll_proto     <- make_proto(kea_fns, proto_params $ coll,       'collections')






# -------------------------------- Type Constructor -------------------------------- #


#' x_
#'
#' Make methods available to a value.
#'
#' @details
#' 	Kea allows functions to be used in both nested form - \bold{xLenOf(xRepeat(3, 'a'))} - and using method-chaining
#'     \bold{x_('a') $ xRepeat(3) $ x_LenOf()}. These two syntaxes are equivalent, but method-chaining syntax is better when
#'     you need to apply many function to an initial value: balancing parentheses is easier and you read the function calls from
#'     left to right instead of from the inside out.
#'
#'     To make methods available to values you need to wrap them in a kea object using \bold{x_}. This tags the value as being of class
#'     'kea', which gives it access to any appropriate (see below) methods. For example,
#'
#'     \code{letters_ <- x_(letters)}
#'
#'     attaches the class 'kea' to the character vector a, b, ... ,z and saves the tagged value to the variable 'letters_'. I use
#'     the convention of always adding an underscore suffix to kea variables, but this convention is not mandatory.
#'
#'     Any value can be supplied to \bold{x_}.
#'
#'     \bold{1. Calling Methods}
#'
#'      The methods available to a kea object depend on the the class and properties of the value given to \bold{x_}. Kea objects containing
#'     arbitrary values can use methods which have parametres named 'val', 'val1', or 'val2'. For example, \bold{xIdentity} and \bold{xIsTrue} both
#'     have parametres named 'val', and as such they can be used on functions,
#'     lists, vectors and any other value you might want to use.
#'
#'     \bold{x_(sqrt) $ xIdentity()}
#'     \bold{x_(1:10) $ xIsTrue()}
#'
#'     Most methods are more selective than this. For example, you can't treat a function as a file path, so the method isn't even available
#'     to functions.
#'
#'     \code{x_(sqrt) $ xRead()}
#'
#'     Which parametre is the kea object given to? That depends on the particular method being called; if there is only one parametre that
#'     a value can be used for is used as that parametre. For example, these following two calls are unambigious and identical.
#'
#'     \code{x_(sqrt) $ xMap(1:10)}
#'     \code{x_(1:10) $ xMap(sqrt)}
#'
#'     Here the function 'sqrt' is matched with the parametre 'fn', and the collection 1:10 is matched with the parametre 'coll'.
#'
#'     For some functions it makes sense to use the left-hand-side kea object as multiple parametres. For example, \bold{xIsIn} an arbitrary value and
#'     a collection as arguments, so both the following examples make sense.
#'
#'     \code{x_(1:3) $ xIsIn(1L)}
#'     \code{x_(1L) $ xIsIn(coll = 1:3)}
#'
#'     In this case you need to explicitely name \bold{1:3} as 'coll', or it is bound to the parametre 'val' instead.
#'
#'     \bold{2. Spell-Checking}
#'
#'     If you mistype a method name kea will attempt to suggest a similar method name, if one exists.
#'
#'     \bold{3. Self-References}
#'     Kea v0.48.0 added an experimental feature which allows methods to use the left-hand side kea object as a non-fixed argument.
#'     For example, it allows statements like
#'
#'     \code{x_(1:3) $ xJoin_(list(self, self)}
#'
#'    . The keyword self is replaced by the value 1:3, and the argument to \bold{xJoin} is then evaluated. This feature still has bugs,
#'    and will be improved over time.
#'
#' @param
#' 	val an arbitrary value. The value to wrap in a kea object.
#'
#' @param
#'    ... see above.
#'
#' @return
#'     An object of class 'kea' which wraps the value supplied to x_. Internally, a 'kea' object is represented
#'     as a list with a single field - \bold{x} - but this field should never be accessed directly.
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
	# Collection any -> Kea any
	# type constructor for the method-chaining data type.



	# -- a useful corner case; there are no methods
	# -- specifically for kea objects with kea
	# -- objects in them. Makes defining methods easier.
	if (any(class(val) == 'kea')) {
		val
	} else {
		# -- cannot just be a val with a class label,
		# -- as if val is null then x_ will fail.
		structure(list(x = val), class = 'kea')
	}
})

#' @rdname x_
#' @export

x__ <- function (...) {
	x_(list(...))
}





# NOTE: Cannot make more specific;
#
# making coll more specific screws up the predictability of length-zero corner cases,
# requires extremely expensive type-checking to be fully generic.

get_proto_ref <- local({

	x_table_members    <- ls(x_table_proto)
	x_factor_members   <- ls(x_factor_proto)
	x_function_members <- ls(x_function_proto)
	x_coll_members     <- ls(x_coll_proto)
	x_any_members      <- ls(x_any_proto)

	function (val) {
		# get the reference to the appropriate methods.

		# -- keep this code fairly efficient.

		if (is.function( val )) {
			list(x_function_proto, x_function_members)
		} else if (is.matrix( val ) || is.data.frame( val )) {
			list(x_table_proto, x_table_members)
		} else if (is.factor( val )){
			list(x_factor_proto, x_factor_members)
		} else if (is_atomic( val ) || is_generic( val )) {
			list(x_coll_proto, x_coll_members)
		} else {
			list(x_any_proto, x_any_members)
		}
	}

})










# suggest_similar_method :: <character> ->
#
# suggest_similar_method takes a mispelled method name, and
# returns the name of the most similar method.
# It uses a combination of nearest-string
# search, and manual hacks.
#

suggest_similar_method <- local({

	# close_method :: <character> -> <character> -> <character>
	#
	# Get the string distance to other candidate methods, and return
	# sufficiently close methods.

	close_method <- function (method_name, candidates) {

		dists <- adist(method_name, candidates)

		meets_threshold <-
			min(dists) < ceiling(nchar(method_name) / log(nchar(method_name)) )

		if (meets_threshold) {
			candidates[which.min(dists)]
		} else {
			character(0)
		}
	}

	# change_of_suffix :: <character> -> <character> -> <character>
	#
	# Transforms the suffix of a method and checks if it equals
	# after changing the suffix.
	#
	# xMethod -> xMethodOf | xMethodOf -> xMethod

	change_of_suffix <- function (method_name, candidates) {

		with_suffix    <- gsub('_$', 'Of_', method_name)
		without_suffix <- gsub('Of_$',  '', method_name)

		if (any(candidates == with_suffix)) {
			with_suffix
		} else if (any(candidates == without_suffix)) {
			without_suffix
		}
	}

	# change_to_prefix :: <character> -> <character> -> <character>
	#
	# Chage the prefix
	#
	# xAsMethod -> xToMethod | xToMethod -> xAsMethod

	change_to_prefix <- function (method_name, candidates) {

		with_prefix    <- gsub('As', 'To', method_name)
		without_prefix <- gsub('To', 'As', method_name)

		if (any(candidates == with_prefix)) {
			with_prefix
		} else if (any(candidates == without_prefix)) {
			without_prefix
		}
	}

	# change_to_keys :: <character> -> <character> -> <character>
	#
	# Change occurrences of the substring 'Names' to 'Keys', as
	# kea only uses Keys.

	change_to_keys <- function (method_name, candidates) {

		swapped <- gsub('Names', 'Keys', method_name)

		if (any(candidates == swapped)) {
			swapped
		}
	}

	# change_common_name :: <character> -> <character> -> <character>
	#
	# Change_common_name checks if a method has a name commonly used in another language.

	change_common_name <- function (method_name, candidates) {

		alias <- function (from, to) {

			out <- list()

			for (incorrect in from) {
				out <- list(out,
					list(as_chaining(method_name),    as_chaining(to)),
					list(as_unchaining(method_name),  as_unchaining(to)),
					list(as_variadic(method_name),    as_variadic(to)),
					list(as_nonvariadic(method_name), as_nonvariadic(to))
				)
			}

			out
		}

		match <- Filter(
			function (pair) {
				pair[[1]] == method_name
			},
			c(
				alias(c('xFilterNot', 'xRemove'), 'xReject'))
		)

		match[[2]]
	}





	function (val, method_name, content_type, invoking_call) {

		# -- get the edit distance to each
		# -- to each method in the prototype.
		proto <- get_proto_ref(val)

		candidates <- setdiff(proto[[2]], 'private')

		# -- check if the function does exist, just not in this
		# -- prototype.

		all_prototypes <- list(
			x_table_proto,
			x_factor_proto,
			x_function_proto,
			x_coll_proto,
			x_any_proto
		)

		if (method_name != 'private') {

			with_method <- Reduce(
				function (acc, proto) {

					members <- ls(proto)

					if (any(method_name == members)) {
						c(acc, proto $ private $ contents_are)
					} else {
						acc
					}

				},
				all_prototypes,
				c()
			)

			if (length(with_method) > 0) {

				message <-
					"Could not find the method " %+% dQuote(method_name) %+%
					" in the methods available for " %+% content_type %+% ", but it was " %+%
					"available for " %+% toString(with_method) %+% '.'

				throw_kea_error(invoking_call, message)

			}

		}


		# -- try to find a similar method.
		matches    <- list(
			close =
				close_method(method_name, candidates),

			change_of_suffix =
				change_of_suffix(method_name, candidates),
			change_to_prefix =
				change_to_prefix(method_name, candidates),
			change_common_name =
				change_common_name(method_name, candidates)
		)

		similar <- matches[[ which(nchar(matches) > 0)[1] ]]

		# -- build up the message.

		message <- if (length(similar) == 0) {
			"Could not find the method " %+% dQuote(method_name) %+%
			" in the methods available for " %+% content_type %+% "."
		} else {
			"Could not find the method " %+% dQuote(method_name) %+%
			" in the methods available for " %+% content_type %+%
			":\n" %+%
			colourise $ green(
				"did you mean " %+% rsample(similar, size = 1) %+% "?")
		}

		throw_kea_error(invoking_call, message)
	}
})






#' @export

`$.kea` <- local({

	# some methods are known by their more common
	# but worse names (like filter, filterNot).
	# Meet the user half way and suggest the "proper" name.

	function (obj, method) {
		# Kea a -> symbol -> function
		# return an kea method associated with the type a.

		method_name <- paste0(substitute(method))
		proto       <- get_proto_ref( obj[['x']] )

		if ( !any(proto[[2]] == method_name) || method_name == "private" ) {
			# -- the invoked method wasn't found, so we should give a suggestion.

			# -- required for proper call formatting in output.
			invoking_call <- call('$', sys.call()[[2]], sys.call()[[3]] )

			contents_are <- proto[[1]][['private']][['contents_are']]

			suggest_similar_method(
				obj[['x']], method_name, contents_are, invoking_call)

		}

		fn <- proto[[1]][[method_name]]
		environment(fn)[['Self']] <- function () {
			obj[['x']]
		}

		fn
	}
})

# -------------------------------- Print Method -------------------------------- #

#' @export

print.kea <- function (x, ...) {

	proto        <- get_proto_ref( x[['x']] )
	contents_are <- proto[[1]][['private']] [['contents_are']]

	header <- colourise $ blue(
		'\n[ an kea object with methods for ' %+% contents_are %+% ' ]')

	cat(
		header  %+% '\n\n' %+%
		'$x_()' %+% '\n')

	print(x $ x_Identity(), ...)
}
