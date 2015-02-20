
kea ::: load_test_dependencies(environment())

notrun <- function () {

	int_test(paste(
		'always returns kea-specific errors. Should catch any unhandled errors.',
		'a very powerful, likely to fail test.'
	))

		local({

			fns <- Map(
				function (fn) {
					list( fn, getNamespace('kea')[[fn]] )
				},
				Filter(
					function (fn_name) {
						!any(fn_name == c('xRepeat', 'xRead', 'xWrite', 'xLambda'))
					},
					ls(envir = getNamespace('kea'), pattern = '^x[A-Z]')
				)
			)

			lapply(sample(fns), function (fn_info) {

				fn_name <- fn_info[[1]]
				fn      <- fn_info[[2]]

				FN      <- as.symbol(fn_name)

				number_of_params <- length(formals(fn))
				number_of_args   <- sample.int(number_of_params, 1)

				expr <- bquote({

					over(val) +

					it(paste('never throws a naked error for ', fn_name)) +

					holdsWhen({

							is_large_number <- is.numeric(unlist(val)) && length(val) == 1 && val > 1000

							length(val) < 1000 &&

							!is_large_number &&

							!is.function(val) && # -- stops random functions playing.

							throws_error(do.call( .(FN), rep(list(val), .(number_of_args)) ))
						},
						throws_kea_error(do.call( .(FN), rep(list(val), .(number_of_args)) ))
					) +

					run(1)

				})





				eval(expr)

			})

		})

}



# -- this is fairly willing to exhaust all system memory, so it's commented out
# -- to save travis and my own computer from being bricked.

# notrun()
