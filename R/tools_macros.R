
# ----------------------------------------------------------------------------------------------
#
# Must
#
# Kiwi uses macro's instead of higher-order functions to check
# that input is valid. There are good reasons for this. I found that
# higher-order functions ended up failing when they were run, and capturing symbols and
# testing was a nightmare. Functions like missing can only be called at the top level.
# so there are also things possible with macros that just aren't with higher-order functions.
#
# Macro's just get injected into the code, so a lot of runtime errors are made into
# build-time errors, in particular missing variables and misbound variables are
# much easier to find. The code can also be visually inspected without
# crawling throw ten functions.

# There are two components;
#
# Must: an object containing macros. These return expressions that
#     that test for some property. These are injected into the document.

Must <- local({

	this <- Object()



	this $ Be_Collection <-
		function (COLL) {
			# this macro expands to check if a value is a collection.

			COLL <- substitute(COLL)

			bquote(if (
				identical('kiwi', class( .(COLL) )) ||
				!is_atomic( .(COLL) ) &&
				!is_generic( .(COLL) )) {

				message <-
					"The argument matching " %+% ddquote( .(COLL) ) %+%
					" must be a list, a pairlist or a typed vector."

				if (any(class( .(COLL) ) == 'kiwi')) {
					message <- message %+%
						"The argument was of class " %+% dQuote("kiwi") %+%
						". Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
						summate( .(COLL) )
				} else {
					message <- message %+% summate( .(COLL) )
				}

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Collection_Of_Collections <-
		function (COLLS) {

			COLLS <- substitute(COLLS)

			bquote({

				all_elems_are_collection <- all( vapply( .(COLLS) , function (coll) {

					'kiwi' %!in% class(coll) &&
					(is_atomic(coll) || is_generic(coll))

				}, logical(1)) )

				if (!all_elems_are_collection) {

					message <-
						"The argument matching " %+% ddquote( .(COLLS) ) %+%
						" must be a collection of lists, vectors or pairlists."

					if (any(class( .(COLLS) ) == 'kiwi')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("kiwi") %+%
							". Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
							summate( .(COLLS) )

					} else {
						message <- message %+% summate( .(COLLS) )
					}

					throw_kiwi_error(sys.call(), message)
				}

			})
		}

	this $ Be_Collection_Of_Equal_Length <-
		function (COLLS) {

			COLLS <- substitute(COLLS)

			bquote({

				all_equal <-
					length( .(COLLS) ) == 0 ||
					all(vapply( .(COLLS), function (coll) {
						length(coll) == length( .(COLLS)[[1]] )
					}, logical(1)))

				if (!all_equal) {

					message <-
						"The argument matching " %+% ddquote( .(COLLS) ) %+%
						" must be a collection of collections with equal lengths." %+%
						summate( .(COLLS) )

					throw_kiwi_error(sys.call(), message)
				}
			})

		}

	this $ Be_Collection_Of_Fn_Matchable <-
		function (COLL) {

			COLL <- substitute(COLL)

			bquote({

				all_match <- all( vapply( .(COLL) , function (val) {

					is.function(val) ||
					(is.character(val) && length(val) == 1) ||
					is.name(val)

				}, logical(1)) )

				if (!all_match) {

					message <-
						"The argument matching " %+% ddquote( .(COLL) ) %+%
						" must be a collection of functions, or symbols or strings" %+%
						" that can be looked up as functions."

					contains_kiwi <- any( vapply( .(COLL), function (elem) {
						any(class( .(COLL) ) == "kiwi")
					}, logical(1)) )

					if (contains_kiwi) {

						message <- message %+%
							"The collection supplied contained kiwi objects. " %+%
							"Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
							summate( .(COLL) )

					} else {
						message <- message %+% summate( .(COLL) )
					}

					throw_kiwi_error(sys.call(), message)
				}

			})
		}

	this $ Be_Collection_Of_Lengths_In_Range <-
		function (COLLS, LOWER, UPPER) {

			COLLS <- substitute(COLLS)
			LOWER <- substitute(LOWER)
			UPPER <- substitute(UPPER)

			bquote( if (any(vapply( .(COLLS), function (coll) {

				.(LOWER) > length(coll) || length(coll) > .(UPPER)

			}, logical(1) )) ) {

				message <-
					"The argument matching " %+% ddquote( .(COLLS) ) %+%
					" must be a collection with lengths in the range " %+%
					.(LOWER) %+% " to " %+% .(UPPER) %+% "." %+%
					summate( .(COLLS) )

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Equal_Length_To <-
		function (COLL1, COLL2) {

			COLL1 <- substitute(COLL1)
			COLL2 <- substitute(COLL2)

			bquote(if (length( .(COLL1) ) != length( .(COLL2) )) {

				message <-
					"The argument matching " %+% ddquote( .(COLL1) ) %+%
					" must be equal length to the argument matching " %+% ddquote( .(COLL2) ) %+% "." %+%
					summate( .(COLL1) )

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Existing_Ref <-
		function (SYM) {

			SYM <- substitute(SYM)

			bquote(if ( !exists( .(SYM), envir = parent.frame()) ) {

				message <-
					"The variable referenced by the symbol " %+% ddquote( .(SYM) ) %+%
					" does not exist."

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Flag <-
		function (BOOL, PRED) {
			# this macro expands to check if a value is True, False or Na.

			BOOL <- substitute(BOOL)
			PRED <- substitute(PRED)

			bquote(if (!is.logical( .(BOOL) ) || length( .(BOOL) ) != 1) {

				message <-
					"The predicate function " %+% ddquote( .(PRED) ) %+%
					" produced a non-{True, False, Na} value." %+%
					summate( .(BOOL) )

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_File <-
		function (STR) {

			STR <- substitute(STR)

			bquote(if (!file.exists( .(STR) )) {

				message <-
					"The argument matching " %+% ddquote( .(STR) ) %+%
					" must be a path to an existing file."

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Fn_Matchable <-
		function (VAL) {
			# this macro expands to check if a value is a function or
			# can be looked up as a function.

			VAL <- substitute(VAL)

			bquote(if (
				!is.function( .(VAL) ) &&
				!is.name( .(VAL) ) &&
				!(is.character( .(VAL) ) && length( .(VAL) ) == 1)) {

					message <-
						"The argument matching " %+% ddquote( .(VAL) ) %+%
						" must be a function, or a string or symbol naming a function."

					if (any(class( .(VAL) ) == 'kiwi')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("kiwi") %+%
							". Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
							summate( .(VAL) )
					} else {
						message <- message %+% summate( .(VAL) )
					}

					throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Indices <-
		function (NUMS, COLL) {

			NUMS <- substitute(NUMS)
			COLL <- substitute(COLL)

			bquote(if (any( .(NUMS) > length( .(COLL) ) | .(NUMS) < -length( .(COLL) ) )) {

				message <-
					"The argument matching " %+% ddquote( .(NUMS) ) %+%
					" must be positive indices of the collection matching " %+% ddquote( .(COLL) ) %+% "." %+%
					summate( .(NUMS) )

				throw_kiwi_error(sys.call(), message)

			})
		}

	this $ Be_Of_Length <-
		function (COLL, LENGTHS) {
			# this macro expands to check that a collection has a certain length.

			COLL    <- substitute(COLL)
			LENGTHS <- substitute(LENGTHS)

			bquote(if (length( .(COLL) ) %!in% .(LENGTHS)) {

				message <-
					"The argument matching " %+% ddquote( .(COLL) ) %+%
					" must have length" %+% paste( .(LENGTHS, collapse = ' or ') ) %+% "." %+%
					summate( .(COLL) )

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Lequal_Than <-
		function (COLL, LENGTH) {
			# this macro expands to check that a collection is lequal than a certain length.

			COLL   <- substitute(COLL)
			LENGTH <- substitute(LENGTH)

			bquote(if (!(length( .(COLL) ) >= .(LENGTH) )) {

				message <-
					"The argument matching " %+% ddquote( .(COLL) ) %+%
					" must have at least " %+%  .(LENGTH) %+% " elements." %+%
					summate( .(COLL) )

				throw_kiwi_error(sys.call(), message)
			})

		}

	this $ Be_Longer_Than <-
		function (LENGTH, COLL) {
			# this macro expands to check that a collection is longer than a certain length.

			COLL   <- substitute(COLL)
			LENGTH <- substitute(LENGTH)

			bquote(if (!(length( .(COLL) ) > .(LENGTH) )) {

				message <-
					"The argument matching " %+% ddquote( .(COLL) ) %+%
					" must have more than " %+%  .(LENGTH) %+% " elements." %+%
					summate( .(COLL) )

				throw_kiwi_error(sys.call(), message)
			})

		}

	this $ Be_Matchable <-
		function (SYM) {
			# this macro expands to test if a value is a symbol.

			SYM <- substitute(SYM)

			bquote({

				if (!is.name( .(SYM) ) && (!is.character( .(SYM) ) || length( .(SYM) ) != 1)) {

					message <-
						"The argument matching " %+% ddquote( .(SYM) ) %+%
						" must be a symbol or a string."

					# -- this might not work with lazy evaluation.
					if (any(class( .(SYM) ) == 'kiwi')) {
						message <- message %+%
							"The argument was of class " %+% dQuote("kiwi") %+%
							". Did you use the wrong form of kiwi method (xMethod vs xMethod_)?" %+%
							summate( .(SYM) )
					} else {
						message <- message %+% summate( .(SYM) )
					}

					throw_kiwi_error(sys.call(), message)
				}
			})


		}

	this $ Be_Named <-
		function (COLL) {

			COLL <- substitute(COLL)

			bquote({

				if ( is.null(names( .(COLL) )) ) {

					message <-
						"The argument matching " %+% ddquote( .(COLL) ) %+%
						" must be named." %+%
						summate( .(COLL) )

					throw_kiwi_error(sys.call(), message)
				}
			})
		}


	this $ Be_Parametres_Of <-
		function (STRS, FN) {
			# this macro expands to check if a set of names are parametres of a function.

			STRS <- substitute(STRS)
			FN   <- substitute(FN)

			bquote(if (any( .(STRS) %!in% names(formals( .(FN) )) )) {

				message <-
					"The argument matching " %+% ddquote( .(STRS) ) %+%
					" must be parametres of the function matching " %+% ddquote( .(FN) ) %+% "." %+%
					summate( .(STRS) )

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Be_Positive_Indices <-
		function (NUMS, COLL) {

			NUMS <- substitute(NUMS)
			COLL <- substitute(COLL)

			bquote(if (any( .(NUMS) > length( .(COLL) ) | .(NUMS) < 1 )) {

				message <-
					"The argument matching " %+% ddquote( .(NUMS) ) %+%
					" must be positive indices of the collection matching " %+% ddquote( .(COLL) ) %+% "." %+%
					summate( .(NUMS) )

				throw_kiwi_error(sys.call(), message)

			})
		}



	this $ All_Be_Whole <-
		function (NUMS) {
			bquote({
				if (FALSE) {
					stop('')
				}
			})
		}

	this $ Not_Be_Missing <-
		function (VAL) {
			# this macro expands to check if a parametre is not missing.

			VAL <- substitute(VAL)

			bquote(if (missing( .(VAL) )) {

				message <-
					"The parametre " %+% ddquote( .(VAL) ) %+%
					" is required but was missing."

				throw_kiwi_error(sys.call(), message)
			})

		}

	this $ Not_Be_Primitive <-
		function (FN) {
			# this macro expands to check if a function is non-primitive.

			FN <- substitute(FN)

			bquote(if (is.primitive( .(FN) )) {

				message <-
					"The argument matching " %+% ddquote( .(FN) ) %+%
					" must be a non-primitive function." %+%
					summate( .(FN) )

				throw_kiwi_error(sys.call(), message)
			})
		}

	this $ Have_Canonical_Arguments <-
		function () {
			# check that the argument names in ... don't
			# clash with the parametre names of a function.

			bquote({

				# check that the names aren't interpreted differently
				# between sys.call (your interpretation of the arguments) and
				# match.call (the system's interpretation). This fixed an
				# odd issue in R"s function call semantics

				# xFix_(function (fn, b) fn(b), fn = xI)

				# is misinterpreded by R; fn is not used as an ellipsis arg, but an arg to xFix.
				# libs like plyr use .fn to try get around this; Kiwi uses this odd middleware macro.
				# It'll throw an error for argument lists that are ambigious.

				.sys_call   <- sys.call()
				.match_call <- match.call()

				if (!all( names(.sys_call) == names(.match_call) | names(.sys_call) == '')) {

					fn_name <- .sys_call[[1]]

					if (length(fn_name) == 1) {
						fn_name <- paste0(fn_name)
						suggested <- gsub('_', '', fn_name)
					} else {
						fn_name   <- 'xMethod_'
						suggested <- 'xMethod'
					}

					invoked <- names(.sys_call)
					matched <- names(.match_call)

					ambigious <- invoked[which(invoked != matched & invoked != '')]

					message <-
						"The ellipsis argument explicitly named " %+% dQuote(ambigious) %+% " matches a parametre " %+%
						"of " %+% dQuote(fn_name)  %+% ". This will be misinterpreted by R. Use " %+%
						dQuote(suggested) %+% " instead of " %+% dQuote(fn_name) %+% "."

					throw_kiwi_error(.sys_call, message)
				}

			})
		}

	this
})

# MakeFun:
# this injects the code into the document, by evaluating the contents of MACRO.












bmacro <- function (expr) {

	parent_frame <- parent.frame()

	unquote <- function (inner) {

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

	# -- generate a fix macro to inject

	eval(unquote(substitute(expr)), parent_frame)
}
