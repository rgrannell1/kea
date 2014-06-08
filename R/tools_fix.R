
# -- an efficient implementation of fix, lacking safe-guards.

fix <- local({

	fn_symbol <- as.symbol('fn')
	arg_keys  <- c('arg1', 'arg2', 'arg3')

	function (fn, coll) {
		# MUST be called with fixed arguments!
		# coll MUST be a list

		fn_formals  <- formals(fn)
		fn_params   <- names(fn_formals)

		# -- interpret positional / named arguments; may be possible to remove.

		coll_names  <- names(coll)

		do.call('function', list(
			# -- select the non-used parametres.
			as.pairlist(fn_formals[ !(arg_keys %in% coll_names) ]),
			bquote({

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
