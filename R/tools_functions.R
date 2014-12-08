
# Package Internals
#
# Documentation for the internals of Kea.
# Every sufficiently large utility library will contain its own utility library.
# These functions are required to reduce repetition of code when implementing kea
# functions.
#
# @keywords internal
# @rdname pkg-internal

# --------------------- shorthand logical functions --------------------- #
# these are exported by kea seperately.

Truth <- function (...) {
	TRUE
}
Falsity <- function (...) {
	FALSE
}

# --------------------- safe replacements --------------------- #

# -- join_exprs
# --
# -- join two expressions into a composite expression.

join_exprs <- local({

	brace <- as.symbol('{')

	function (expr1, expr2) {

		if (is.null(expr2)) {
			return(expr1)
		} else if (is.null(expr1)) {
			return(expr2)
		}

		list_expr1 <- as.list(expr1)
		list_expr2 <- as.list(expr2)

		has_brace1 <- list_expr1[[1]] == '{'
		has_brace2 <- list_expr2[[1]] == '{'

		if (has_brace1) {
			if (has_brace2) {
				as.call( c(list_expr1, list_expr2[-1]) )
			} else {
				as.call(c(list_expr1, expr2))
			}
		} else {
			if (has_brace2) {
				as.call( c(brace, expr1, list_expr2[-1])  )
			} else {
				as.call( c(brace, expr1, 	expr2)  )
			}
		}
	}
})

# substitute_q
#
# Hadley Wickham's substitute_q, which substitutes x once.

substitute_q <- function(x, env) {
	call <- substitute(substitute(x, env), list(x = x))
	eval(call)
}









# --------------------- misc. tools --------------------- #

# @section one_of:
#
# Return one value from a collection.
#
# @keywords internal
# @rdname pkg-internal

one_of <- function (coll) {
	# coll [any] -> any
	# select a single value from a collection.

	ith <- rsample(seq_len(length(coll)), size = 1)
	coll[[ith]]
}

# @section equals:
#
# R's equal operator doesn't work on lists or strange values.
# equals is a better measure of identity.
#
# @keywords internal
# @rdname pkg-internal

'%is%' <- function (a, b) identical(a, b)

as_formals <- function (params) {

	as.pairlist(structure(
		rep(list(quote(expr=)), length(params)),
		names = params))

}

#
#
#
#
#

params_of <- function (fn) {

	if (is.primitive(fn))
		names(as.list( head(as.list(args(fn)), -1) ))
	else
		names(as.list( formals(fn) ))

}


#
#
#
#
#

pluralise <- function (str, num) {

	if (is.na(num)) {
		stop('internal error: pluralise was given an NA value ', num)
	}

	if (round(num) != num) {
		stop('internal error: pluralise was given a non-round number ', num)
	}

	if (length(str) != 1) {
		stop('internal error: pluralise was given a length ', length(str), ' string ', str)
	}

	if (num == 0 || num > 1) {
		paste0(str, 's')
	} else if (num == 1) {
		str
	}
}

# @section call_with_params:
#
# Construct a call to a function 'fnname' with the parametres of
# a second function. Useful for higher order functions.
#
# @keywords internal
# @rdname pkg-internal

call_with_params <- function (fnname, fn) {
	# string -> function -> call
	# create call for a function with
	# the arguments of another function.

	as.call(
		lapply(
			c(fnname, names(xFormalsOf(fn)) ),
			as.symbol))
}

# @section +:
#
# Concatenate two strings.
#
# @keywords internal
# @rdname pkg-internal

"%+%" <- function (x, y) {
	paste0(x, y, sep = "")
}

# @section in:
#
# An infix function to test for the non-membership of an element in a set.
#
# @keywords internal
# @rdname pkg-internal

'%not_in%' <- function (coll1, coll2) {

	if (length(coll1) == 0) {
		logical(0)
	} else if (length(coll2) == 0) {
		# -- the base function does this; should the replacement?
		True
	} else {

		vapply(coll1, function (elem1) {

			!any( vapply(coll2, function (elem2) {
				identical(elem1, elem2)
			}, logical(1)) )

		}, logical(1), USE.NAMES = False)

	}
}

# to dedottify my code.
match_fn <- match.fun

# -- evaluate a dangerous expression, on error return a default value.
tryDefault <- function (expr, val) {
	tryCatch(
		expr,
		warning = function (warn) val,
		error   = function (err)  val
	)
}

# -- set a field on list.
#

add_field <- function (coll, field, val) {
	coll [[field]] <- val
	coll
}

# --------------------- environment manipulation --------------------- #

# @section Object:
#
# Construct an empty environment.
#
# @keywords internal
# @rdname pkg-internal

Object <- function () {
	# construct an empty environment.

	new.env(parent = emptyenv())
}






# --------------------- property tests --------------------- #

# @section is_collection:
#
# Is a value a generic or atomic vector or a pairlist.
#
# @keywords internal
# @rdname pkg-internal

is_collection <- function (val) {
	# is a value a pairlist, list or typed vector?

	is_generic(val) || is_atomic(val)
}

# --------------------- testing & message functions --------------------- #

ddparse <- function (val, collapse = "") {
	# safely deparse a string.

	paste0(deparse(val), collapse = collapse)
}

ddquote <- function (sym) {
	# -- wrap a symbol or string in quotation marks.
	# -- deparse the substituted symbol, to make sure that the output is length-one

	dQuote( ddparse(substitute(sym)) )
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
	# appropriate suffix (1th, 2nd, 3rd, ...)
	# useful for error messages.

	# -- just in case...
	if (num == Inf) {
		return("infinith")
	} else if (num == -Inf) {
		return("-infinith")
	}

	last <- as.numeric(substr(
		paste(num),
		nchar(paste(num)),
		nchar(paste(num)) ))

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






# -- load the internal tools needed for testing through assign.
# -- used in all the testing functions.

# -- envir cannot be defaulted to environment( ),
# -- it will break the expected behaviour.

test_type <- function (type) {
	function (...) {
		message(paste0(..., " - ", type, " tests", collapse = ''))
	}
}

unit_test <- test_type('unit')
int_test  <- test_type('integration')





load_test_dependencies <- function (envir) {

	deps <-
		list(
			over            = over,
			it              = it,

			holdsWhen       = holdsWhen,
			worksWhen       = worksWhen,
			failsWhen       = failsWhen,

			holdsFor        = holdsFor,
			worksFor        = worksFor,
			failsFor        = failsFor,

			unit_test       = unit_test,
			int_test        = int_test,

			run             = run,
			suchThat        = suchThat,
			grasp           = grasp,

			`+.xforall`     = `+.xforall`,

			is_collection   = is_collection,

			# temporary
			`%is_in%`       = '%in%',

			MakeFun         = MakeFun,

			is_atomic       = is_atomic,
			is_generic      = is_generic,
			as_named        = as_named,
			is_named        = is_named,
			is_alphanumeric = function (str) {
				chars <- strsplit(str, '')[[1]]

				length(setdiff(chars, c(
					"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
					"m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y",
					"z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
					"M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y",
					"Z", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
				)) == 0
			}
		)

	for (key in names(deps)) {
		assign(key, deps[[key]], envir = envir)
	}
}
