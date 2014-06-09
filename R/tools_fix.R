
# ----------------------------------------------------------------------------------------------
#
# fix
#
# An efficient implementation of xFix, lacking safe-guards and some features.
# The is used internally when a function is called with only some of its arguments.

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

Fix <- function (FN, SYM1, SYM2, SYM3) {

	# -- marginally more performant than length(which( ))

	len_args <- (!missing(SYM1)) + (!missing(SYM2)) + (!missing(SYM3))

	# -- get the symbols for the parametres passed to Fix (if given).
	invoking_call <- as.list(sys.call())[-1]

	FN <- invoking_call[[1]]

	if (!missing(SYM1)) {
		SYM1 <- invoking_call[[2]]
	}
	if (!missing(SYM2)) {
		SYM2 <- invoking_call[[3]]
	}
	if (!missing(SYM3)) {
		SYM3 <- invoking_call[[4]]
	}

	if (len_args == 1) {
		# -- the simplest case; if no arguments are given return the
		# -- called function unchanged.

		bquote(
			if (missing( .(SYM1) )) {
				# _
				return( .(FN) )
			}

			# |
			# -- otherwise run

		)

	} else if (len_args == 2) {

			.this   <- sys.function()
			.params <- names( formals(.this) )

			.arguments <- list()

			for (.ith in seq_along(.params)) {
				# -- if an argument is given
				if ( !do.call(missing, list( .params[[.ith]] )) ) {

					# -- append the argument to a list.
					.arguments[[ paste0('arg', .ith) ]] <- eval(as.symbol( .params[[.ith]] ))
				}
			}

			# -- no arguments given; return this function unchanged.
			if (length(.arguments) == 0) {
				return (.this)
			} else if (length(.arguments) != length(.params) ) {
				return(fix(.this, .arguments))
			}

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

						return ( fix( .(FN), list(arg3 = .(SYM3) )) )
					}
				} else{

					if (missing_3) {
						# _|_
						# -- first and third missing; fix second.

						return ( fix( .(FN), list(arg2 = .(SYM2) )) )

					} else {
						# _||
						# - first missing; fix second and third.

						return ( fix( .(FN), list(arg2 = .(SYM2), arg3 = .(SYM3) )) )
					}

				}

			} else if (missing_2) {

				if (missing_3) {
					# |__
					# -- second and third missing; set first.

					return ( fix( .(FN), list(arg1 = .(SYM1) )) )
				} else {
					# |_|
					# -- first and third missing; fix second.

					return (fix( .(FN), list(arg1 = .(SYM1), arg3 = .(SYM3)) ))
				}

			} else if (missing_3) {
				# ||_
				# -- third missingl set first and second.

				return (fix( .(FN), list(arg1 = .(SYM1), arg2 = .(SYM2)) ))
			}

			# |||
			# -- run in this case

		})

	} else {
		stop('internal error in Fix.')
	}

}
