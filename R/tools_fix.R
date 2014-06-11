
# -------------------------------- fix -------------------------------- #
#
# fix
#
# An efficient implementation of xFix, lacking safe-guards and some features.
# This is used internally when a function is called with only some of its arguments.

fix <- local({

	fn_symbol <- as.symbol('fn')
	arg_keys  <- c('arg1', 'arg2', 'arg3')

	function (fn, coll) {
		# MUST be called with fixed arguments!
		# coll MUST be a list

		fn_formals <- formals(fn)
		fn_params  <- names(fn_formals)

		# -- interpret positional / named arguments; may be possible to remove.

		coll_names  <- names(coll)
		do.call('function', list(
			# -- select the non-used parametres.
			as.pairlist(fn_formals[ arg_keys[seq_along(fn_formals)] %!in% coll_names ]),
			bquote({
				"A partially applied function."

				# -- check if each parametre has a 'positional' matching argument,
				# -- using the pseudoindices arg1, arg2, arg3.
				.( as.call(c(fn_symbol, lapply(seq_along(fn_params), function (ith) {

					# -- the ith parametre has a matching argument.
					if (any( coll_names == arg_keys[[ith]] )) {
						coll[[ arg_keys[[ith]] ]]
					} else {
						as.symbol( fn_params[[ith]] )
					}

				}) )) )

			})

		))

	}
})


# -------------------------------- Fix -------------------------------- #
#
#' An efficient implementation of xFix, lacking safe-guards and some features.
#' This is used internally when a function is called with only some of its arguments.
#'
#' @param FN   a symbol, used as a reference to the function to fix.
#'
#' @param SYM1 a symbol, the name of the first parametre.
#' @param SYM2 a symbol, the name of the first parametre. Optional.
#' @param SYM3 a symbol, the name of the first parametre. Optional.
#'
#' @param PRE1 an expression. Input checks that are run exclusively
#' on the first argument. Optional.
#'
#' @param PRE2 an expression. Input checks that are run exclusively
#' on the second argument. Optional.
#'
#' @param PRE3 an expression. Input checks that are run exclusively
#' on the third argument. Optional.
#'
#' @param PRE an expression. Input checks that need more than one
#' argument at once (checking if two lengths are equal for example). Optional.
#'
#' @param FINAL an expression. An arbitrary code block to run before running
#' the function body.

Fix <- function (FN, SYM1, SYM2, SYM3, PRE1, PRE2, PRE3, FINAL) {

	# -- marginally more performant than length(which( ))
	len_args <- (!missing(SYM1)) + (!missing(SYM2)) + (!missing(SYM3))

	# -- get the symbols for the parametres passed to Fix (if given).
	invoking_call <- as.list(match.call())[-1]

	FN <- invoking_call[[1]]

	# -- rebind arguments as their lazy form with match.call

	# -- the use of SPREAD_PARAMETRE instead of ...
	# -- directly appears to be essential. Swap the symbol SPREAD_PARAMETRE
	# -- back for ... here.

	if (!missing(SYM1)) {
		SYM1 <- invoking_call $ SYM1

		if (paste0(SYM1) == 'SPREAD_PARAMETRE') {
			SYM1 <- as.symbol('...')
		}
	}

	if (!missing(SYM2)) {
		SYM2 <- invoking_call $ SYM2

		if (paste0(SYM2) == 'SPREAD_PARAMETRE') {
			SYM2 <- as.symbol('...')
		}
	}

	if (!missing(SYM3)) {
		SYM3 <- invoking_call $ SYM3

		if (paste0(SYM3) == 'SPREAD_PARAMETRE') {
			SYM3 <- as.symbol('...')
		}
	}

	if (!missing(PRE1)) {
		PRE1 <- invoking_call $ PRE1
	}
	if (!missing(PRE2)) {
		PRE2 <- invoking_call $ PRE2
	}
	if (!missing(PRE3)) {
		PRE3 <- invoking_call $ PRE3
	}

	if (!missing(FINAL)) {
		FINAL <- invoking_call $ FINAL
	}

	if (len_args == 1) {
		# -- the simplest case; if no arguments are given return the
		# -- called function unchanged.

		bquote({
			if (missing( .(SYM1) )) {
				# _
				return( .(FN) )
			}

			# |
			# -- check the supplied arguments.
			.(if (!missing(PRE1))  PRE1  else NULL)

			.(if (!missing(FINAL)) FINAL else NULL)

			# -- run

		})

	} else if (len_args == 2) {

		bquote({
			# -- more complicated; if no parametres given
			# -- return function unchanged. if both are given
			# -- fix both. Otherwise fix the given parametre
			# --
			# -- 1 + 2 + 1

			missing_1 <- missing( .(SYM1) )
			missing_2 <- missing( .(SYM2) )

			if (missing_1) {
				if (missing_2) {
					# __
					return ( .(FN) )
				} else {
					# _|

					.(if (!missing(PRE2)) PRE2 else NULL)

					return ( fix( .(FN), list(arg2 = .(SYM2) )) )
				}
			} else if (missing_2) {
				# |_

				.(if (!missing(PRE1)) PRE1 else NULL)

				return ( fix( .(FN), list(arg1 = .(SYM1) )) )
			} else {
			# ||

				.(if (!missing(PRE1)) PRE1 else NULL)
				.(if (!missing(PRE2)) PRE2 else NULL)

			}

			.(if (!missing(FINAL)) FINAL else NULL)


			# -- run

		})

	} else if (len_args == 3) {

		bquote({
			missing_1 <- missing( .(SYM1) )
			missing_2 <- missing( .(SYM2) )
			missing_3 <- missing( .(SYM3) )

			if (missing_1) {

				if (missing_2) {
					if (missing_3) {
						# -- all three are missing; return the function
						# ___
						return ( .(FN) )
					} else {
						# __|
						# first two missing; fix three

						.(if (!missing(PRE3))  PRE3  else NULL)

						return ( fix( .(FN), list(arg3 = .(SYM3) )) )
					}
				} else{

					if (missing_3) {
						# _|_
						# -- first and third missing; fix second.

						.(if (!missing(PRE2))  PRE2  else NULL)

						return ( fix( .(FN), list(arg2 = .(SYM2) )) )

					} else {
						# _||
						# - first missing; fix second and third.

						.(if (!missing(PRE2))  PRE2  else NULL)
						.(if (!missing(PRE3))  PRE3  else NULL)

						return ( fix( .(FN), list(arg2 = .(SYM2), arg3 = .(SYM3) )) )
					}

				}

			} else if (missing_2) {

				if (missing_3) {
					# |__
					# -- second and third missing; set first.

					.(if (!missing(PRE1))  PRE1  else NULL)

					return ( fix( .(FN), list(arg1 = .(SYM1) )) )
				} else {
					# |_|
					# -- first and third missing; fix second.

					.(if (!missing(PRE1))  PRE1  else NULL)
					.(if (!missing(PRE3))  PRE3  else NULL)

					return (fix( .(FN), list(arg1 = .(SYM1), arg3 = .(SYM3)) ))
				}

			} else if (missing_3) {
				# ||_
				# -- third missingl set first and second.

				.(if (!missing(PRE1))  PRE1  else NULL)
				.(if (!missing(PRE2))  PRE2  else NULL)

				return (fix( .(FN), list(arg1 = .(SYM1), arg2 = .(SYM2)) ))
			} else {
				# |||
				.(if (!missing(PRE1)) PRE1 else NULL)
				.(if (!missing(PRE2)) PRE2 else NULL)
				.(if (!missing(PRE3)) PRE3 else NULL)

			}


			.(if (!missing(FINAL)) FINAL else NULL)

			# -- run in this case

		})

	} else {
		stop('internal error in Fix.')
	}

}
