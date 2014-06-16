
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
