#
# To Developers,
#
# Every sufficiently large utility library will contain its own utility library.
# These functions are required to reduce repetition of code when implementing arrow
# functions.
#
# Most utilities are obvious, but I will outline key utilities.
#

#
# as_typed_vector:
#     A function to try convert a list of values to a typed vector.
#     A list of integers should be interconvertable to an integer vector,
#     if required. This function is used to make sure arrow functions are
#     agnostic to the difference between typed and generic vectors.
#
# try_hof:
#     Arrow functions almost always throw errors that can be quickly debugged.
#     Unfortunately,  higher-order functions throw a spanner in the works by
#     allowing user-written functions to throw odd errows that cannot be
#     located easily.
#
#     try_hof is a variant of tryCatch that allows
#     extra-information to be added to the errors produced by higher-order functions.
#
# summate:
#     Debugging is annoying, so I'd like to provide the attributes of an error
#     causing object.
#

# --------------------- shorthand logical functions --------------------- #

Truth <- function (...) {
	True
}
Falsity <- function (...) {
	False
}
Moot <- function (...) {
	Na
}

# --------------------- misc. tools --------------------- #

one_of <- function (coll) {
	# coll [any] -> any
	# select a single value from a collection.

	ith <- sample(seq_along(coll), size = 1)
	coll[[ith]]
}

'%equals%' <- function (a, b) {
	# are two values identical?

	identical(a, b)
}


call_with_params <- function (name, fn) {
	# string -> function -> call
	# create call for a function with
	# the arguments of another function.

	as.call(
		lapply(
			c(name, names(xFormalsOf(fn)) ),
			as.symbol))
}

"%+%" <- function (x, y) {
	# javascript-style string concatenation.

	paste0(x, y, sep = "")
}

'%!in%' <- function (x, y) {
	!(x %in% y)
}

# to dedottify my code.
match_fn <- match.fun

# --------------------- environment manipulation --------------------- #

Object <- function () {
	# construct an empty environment.

	new.env(parent = emptyenv())
}

join_env <- function (x, y) {
	# do not use this often; it's a very slow
	# way of joining two environments.

	if (missing(x)) {
		exclaim$parametre_missing(x)
	}
	if (missing(y)) {
		exclaim$parametre_missing(y)
	}

	as.environment( c(as.list( x ), as.list( y )) )
}

# --------------------- the arrow container --------------------- #

is_arrow <- function (val) {
	# is a function an arrow object?

	if (missing(val)) {
		exclaim$parametre_missing(val)
	}

	class(val) == "arrow"
}





# --------------------- property tests --------------------- #

is_fn_matchable <- function (val) {
	# is a value a function or matchable as a function?

	is.function(val) || is.symbol(val) ||
	(is.character(val) && length(val) == 1)
}

is_collection <- function (val) {
	# is a value a pairlist, list or typed vector?

	is.vector(val) || is.pairlist(val)
}

is_recursive <- function (val) {
	is.list(val) || is.pairlist(val)
}

maybe_atomic <- function (val) {
	length(val) %in% 0:1
}



# --------------------- coercion functions --------------------- #

as_parametres <- function (names) {
	# takes a string of names and converts them to
	# a pairlist of formals with no defaults.

	structure(
		replicate(length(names), quote(expr=)),
		names = names
	)
}

to_value_unit <- function (coll) {
	# collection -> collection
	# convert a length-zero collection.

	if (length(coll) == 0) {
		if (is.double(coll) || is.integer(coll)) {
			0
		} else if (is.character(coll)) {
			""
		} else if (is.logical(coll)) {
			False
		} else if (is.raw(coll)) {
			as.raw(00)
		} else {
			stop(
				"internal arrow error: " %+% "
				cannot convert to non-implemented vector type.")
		}

	} else {
		coll
	}
}

as_typed_vector <- local({

	type_tests <- list(
		logical =
			is.logical,
		integer =
			is.integer,
		double =
			is.double,
		numeric =
			is.numeric,
		character =
			is.character,
		raw =
			is.raw,
		complex =
			is.complex
	)

	function (coll, mode, value_unit = False) {
		# coerces an R is_collection (pairlist, list, or typed vector)
		# to another mode, if the vector is homogenously typed.
		# this makes list("a") ~ "a", making arrow more generic.

		coll_symbol <- match.call()$coll

		type_test <- type_tests[[mode]]

		if (is.atomic(coll)) {
			# vectors are always homogenous, so we can fast-track checking.

			if (length(coll) == 0) {
				# conversion of empty vectors is always possible.
				coll <- as.vector(coll, mode = mode)
			} else {

				if (!type_test(coll)) {
					stop(exclaim$type_conversion_failed_(coll_symbol, mode, summate(coll)) )
				}

				coll
			}
		} else {
			# generic vectors need more checking.

			all_length_one <- all(sapply(coll, length) == 1)
			is_homogenous <- all(sapply(coll, type_test))

			if (!all_length_one) {
				stop(
					"the argument matching ", toString(coll_symbol),
					" must be a collection of length ", 1, " values.")
			}

			if (!is_homogenous) {
				stop( exclaim$type_conversion_failed_(coll_symbol, mode, summate(coll)) )
			}

			coll <- if (mode == 'raw') {
				unlist(coll)
			} else {
				as.vector(coll, mode = mode)
			}

			coll
		}
	}
})


try_hof <- function (expr, invoking_call) {
	# expression -> call -> any
	# provide a good error message if a higher-order function
	# fails because the user provided a dodgy function.

	tryCatch(
		expr,
		warning = function (warn) {

			calling_fn <- invoking_call[[1]]

			assert(
				False, invoking_call,
				yelp$warning_higher_order(calling_fn, warn))

		},
		error = function (err) {

			calling_fn <- invoking_call[[1]]

			assert(
				False, invoking_call,
				yelp$error_higher_order(calling_fn, err))

		}
	)
}

# --------------------- testing & message functions --------------------- #


ddparse <- function (val, collapse = "") {
	# safely deparse a string.

	paste0(deparse(val), collapse = collapse)
}

ddquote <- function (sym) {
	paste0(dQuote(sym), collapse = '')
}

newline <- function (val) {
	paste0(val, collapse = "\n")
}

wrap <- function (...) {
	# wrap and indent a string,

	paste0(
		strwrap(...),
		collapse = '')
}

ith_suffix <- function (num) {
	# number -> string
	# takes a number i, adds the
	# appropriate suffix (ith, ind, ist)
	# useful for error messages.

	last <- as.numeric(substr(
		toString(num),
		nchar(toString(num)),
		nchar(toString(num)) ))

	suffix <-
		if (num == 2) {
			"nd"
		} else if (num == 3) {
			"rd"
		} else if (num == 11 || num == 12 || num == 13) {
			"th"
		} else if (last == 1) {
			"st"
		} else if (last == 2) {
			"nd"
		} else if (last == 3) {
			"rd"
		} else {
			"th"
		}

	paste0(num, suffix)
}

summate <- local({
	# Returns a string of information about an input object.

	output_key_value_pairs <-
		function (coll) {

			Reduce(
				'%+%',
				lapply(names(coll), function (name) {

					name %+% ':\n' %+%
					'    ' %+% paste0(coll[[name]], collapse = '') %+% '\n'

			}) )
		}

	profile <- Object()

	# --- A --- #
	# --- B --- #
	# --- C --- #

	profile$character_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_empty =
					length(which(nchar(obj) == 0)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing character vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$closure <-
		function (obj) {

			traits <- list(
				is_primitive =
					is.primitive(obj),
				arity =
					if (is.primitive(obj)) {
						length( head(as.list(args(obj)), -1) )
					} else {
						length(formals(obj))
					},
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing function ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- D --- #

	profile$data_frame <-
		function (obj) {

		}

	profile$default <-
		function (obj) {

			traits <- list(
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing value ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$double_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_positive =
					length(which(obj[ !(is.na(obj)) ] > 0)),
				no_zero =
					length(which(obj[ !(is.na(obj)) ] == 0)),
				no_negative =
					length(which(obj[ !(is.na(obj)) ] < 0)),
				no_na =
					length( which(is.na(obj)) ),
				no_nan =
					length( which(is.nan(obj)) ),
				no_whole =
					local({
						roundable <- obj[ !(is.na(obj) | is.nan(obj) | is.infinite(obj)) ]
						length(which(round(roundable) == roundable))
					}),
				no_infinite =
					length( which(is.integer(obj)) ),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing double vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- E --- #
	# --- F --- #

	profile$factor <-
		function (obj) {

			traits <- list(
				ordered_factor =
				is.ordered(obj),
				levels =
					length(levels(obj)),
				length =
					length(obj),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing factor ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- G --- #

	profile$generic_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj)
			)

			"\n\n" %+% "[ properties of the error-causing list or pairlist ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- H --- #
	# --- I --- #

	profile$integer_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_positive =
					length(which(obj[ !(is.na(obj)) ] > 0)),
				no_zero =
					length(which(obj[ !(is.na(obj)) ] == 0)),
				no_negative =
					length(which(obj[ !(is.na(obj)) ] < 0)),
				no_na =
					length( which(is.na(obj)) ),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing integer vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- J --- #
	# --- K --- #
	# --- L --- #

	profile$logical_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_na =
					length( which(is.na(obj)) ),
				no_true =
					length(which(obj)),
				no_false =
					length(which(!obj)),
				classes =
					deparse(class(obj))
			)


			"\n\n" %+% "[ properties of the error-causing logical vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- M --- #

	profile$matrix <-
		function (obj) {

			traits <- list(
				nrow =
					nrow(obj),
				ncol =
					ncol(obj),
				type =
					deparse(typeof(obj)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing matrix ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- N --- #

	profile$null <-
		function (obj) {

			"\n\n" %+% "[ properties of ther error-causing object ]:" %+% "\n\n" %+%
			"NULL"
		}

	# --- O --- #
	# --- P --- #
	# --- Q --- #
	# --- R --- #

	profile$raw_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				classes =
					deparse(class(obj))
			)


			"\n\n" %+% "[ properties of the error-causing raw vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$character_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_empty =
					length(which(nchar(obj) == 0)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing character vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}


	# --- S --- #
	# --- T --- #

	profile$character_vector <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				no_empty =
					length(which(nchar(obj) == 0)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "[ properties of the error-causing character vector ]:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# -- U --- #
	# --- V --- #
	# --- W --- #
	# --- X --- #
	# --- Y --- #
	# --- Z --- #

	# the set of implemented object summaries.

	function (obj) {
		# return the appropriate string summary,
		# depending on the type of object supplied.

		response_pairs <- list(
			list(
				is.function,
				profile$closure),

			list(
				is.null,
				profile$null),

			list(
				is.factor,
				profile$factor),

			list(
				function (x) is.list(x) || is.pairlist(x),
				profile$generic_vector),

			list(
				function (x) is.logical(x) && is.vector(x),
				profile$logical_vector),
			list(
				function (x) is.raw(x) && is.vector(x),
				profile$raw_vector),
			list(
				function (x) is.integer(x) && is.vector(x),
				profile$integer_vector),
			list(
				function (x) is.double(x) && is.vector(x),
				profile$double_vector),
			list(
				function (x) is.character(x) && is.vector(x),
				profile$character_vector),

			list(
				is.matrix,
				profile$matrix )
		)

		for (pair in response_pairs) {

			test <- pair[[1]]
			response <- pair[[2]]

			if (test(obj)) {
				message <- paste0(response(obj), collapse = '')

				stopifnot(length(message) == 1)
				return (message)
			}
		}

	profile$default(obj)
	}
})
