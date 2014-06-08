
# -- an efficient implementation of fix, lacking safe-guards.

fix <- local({

	fn_symbol <- as.symbol('fn')

	function (fn, coll) {
		# MUST be called with fixed arguments!
		# coll MUST be a list

		fn_formals  <- formals(fn)
		fn_params   <- names(fn_formals)

		# -- interpret positional / named arguments; may be possible to remove.

		coll_names  <- names(coll)

		out <- function () {}

		do.call('function', list(
			# -- select the non-used parametres.
			as.pairlist(fn_formals[ !(fn_params %in% names(coll)) ]),
			{}
		))

		if (any(coll_names == 'arg1')) {

			if (any(coll_names == 'arg2')) {
				if (any(coll_names == 'arg3')) {
					# ___
					formals(out) <- NULL
					body(out)    <- as.call(list( fn_symbol, coll[[1]], coll[[2]], coll[[3]] ))
				} else {
					# __|
					formals(out) <- as.pairlist(fn_formals[3])
					body(out)    <- as.call(list( fn_symbol, coll[[1]], coll[[2]], as.symbol( fn_params[[3]] ) ))
				}
			} else{

				if (any(coll_names == 'arg3')) {
					# _|_
					formals(out) <- as.pairlist(fn_formals[2])
					body(out) <- as.call(list( fn_symbol, coll[[1]], as.symbol( fn_params[[2]] ), coll[[3]] ))
				} else {
					# _||
					formals(out) <- as.pairlist(fn_formals[2:3])
					body(out)    <- as.call(list( fn_symbol, coll[[1]], as.symbol( fn_params[[2]] ), as.symbol( fn_params[[3]] )))
				}
			}

		} else if (any(coll_names == 'arg2')) {

			if (any(coll_names == 'arg3')) {
				# |__
				formals(out) <- as.pairlist(fn_formals[1])

				body(out) <- as.call(list( fn_symbol, as.symbol( fn_params[[1]] ), coll[[2]], coll[[3]] ))
			} else {
				# |_|
				formals(out) <- as.pairlist(fn_formals[c(1,3)])
				body(out) <- as.call(list( fn_symbol, as.symbol( fn_params[[1]] ), coll[[2]], as.symbol( fn_params[[3]] )))
			}

		} else if (any(coll_names == 'arg3')) {
			# ||_
			formals(out) <- as.pairlist(fn_formals[1:2])
			body(out) <- as.call(list( fn_symbol, as.symbol( fn_params[[1]] ), as.symbol( fn_params[[2]] ), coll[[3]]))
		}

		out
	}
})
