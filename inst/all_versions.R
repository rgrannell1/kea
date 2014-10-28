
require(git2r,          quietly = TRUE, warn.conflicts = FALSE)
require(devtools,       quietly = TRUE, warn.conflicts = FALSE)
require(kea,            quietly = TRUE, warn.conflicts = FALSE)
require(microbenchmark, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2,        quietly = TRUE, warn.conflicts = FALSE)
require(scales,         quietly = TRUE, warn.conflicts = FALSE)
require(reshape2,       quietly = TRUE, warn.conflicts = FALSE)



kea <- (...) := {
	xImplode(.Platform $ file.sep, xJoin_(getwd(), ...))
}





config <- list(
	username    = 'rgrannell1',
	reponame    = 'kea',
	repo_url    = "https://github.com/rgrannell1/kea",

	benchmarks  = kea("inst/benchmarks"),
	seconds     = 0.5
)






is_current_version <- xIs(xVersion())

on.exit({

	if ( !is_current_version(xVersion()) ) {

		message('-- reinstalling latest version of ', config $ reponame)
		install_github(config $ reponame, config $ username, ref = 'releases')

	}

})


repo_path <- '/tmp/git2r-'
#repo_path <- tempfile(pattern = "git2r-")
#dir.create(repo_path)




#repo  <- clone(config $ repo_url, repo_path)
repo  <- repository(repo_path)






releases <- repo := {

	x_(tags(repo)) $
	xSelect(tag := {
		xIsMatch('^v[0-9]+[.][0-9]+[.][0-9]+$', tag @ name)
	})             $
	xSortBy(tag := {
		as.numeric(xAmend('v|[.]', '', tag @ name))
	})             $
	x_Take(2)
}

# avoid naming conflicts later.
stopwatch <- xStopwatch





benchmarks <-
	x_(list.files(config $ benchmarks, full.names = True)) $
	x_Map(
		xJuxtapose_(xI, parse))




try_load <- (reponame : username : ref : callback) := {

	test_env <- new.env(parent = environment())

	tryCatch(
		eval({

			#install_github(reponame, username, ref = ref, quiet = True)

			# not generic!
			package_number <- as.numeric(gsub('[v]|[.]', '', ref))

			if (package_number < 160) {

				require(arrow, quietly = TRUE, warn.conflicts = FALSE)

			} else if (package_number < 420) {

				require(kiwi,  quietly = TRUE, warn.conflicts = FALSE)

			} else {

				require(kea,   quietly = TRUE, warn.conflicts = FALSE)

			}

			message( "--- successfully installed version ", packageVersion(reponame))

			callback( )

		}, test_env),
		error = err := {
			message('--- failure while loading ', ref)
			message(err)
		},
		warning = warn := {
			message('--- warning while loading ', ref)
			message(warn)
		}
	)

}





try_benchmark <- (benchmarks : ref : seconds) := {

	message( "--- starting benchmarks " )

	lapply(seq_along(benchmarks), function (ith) {

		benchmark <- benchmarks[[ith]]

		benchmark_file   <- benchmark[[1]]
		benchmarks_exprs <- benchmark[[2]]

		lapply(seq_along(benchmarks_exprs), function (jth) {

			test_env   <- new.env(parent = environment())

			expr     <- benchmarks_exprs[[jth]]
			deparsed <- paste0(deparse(expr), collapse = '\n')

			expr_times <- tryCatch(
				eval(bquote({

					expr_times     <- list()
					time_remaining <- stopwatch(seconds)

					while (time_remaining()) {

						# cargo-cult programming.
						gc(verbose = False)
						Sys.sleep(1)

						group_times <- microbenchmark(
							.(expr), unit = 'hz', times = 60, control = list(warmup = 10)) $ time

						expr_times <- c(expr_times, group_times)
					}

					range <- summary(1 / as.numeric(expr_times) * 10^9)

					list(
						file   = benchmark_file,
						expr   = deparsed,
						ref    = ref,

						lower  = range[['1st Qu.']],
						median = range[['Median']],
						upper  = range[['3rd Qu.']]
					)

				}), envir = test_env),

				error = function (err) {

					message('-- failure!')

					list(
						file   = benchmark_file,
						expr   = deparsed,
						ref    = ref,

						lower  = 0,
						median = 0,
						upper  = 0
					)

				},
				warning = function (warn) {

					message('-- warning!')

					list(
						file   = benchmark_file,
						expr   = deparsed,
						ref    = ref,

						lower  = 0,
						median = 0,
						upper  = 0
					)

				}
			)

			expr_times

		})
	})

}




run_benchmarks <- (repo_path : benchmarks: ref) := {
	try_load(config $ reponame, config $ username, ref, function () {
		try_benchmark(benchmarks, ref, config $ seconds)
	})
}

timings <-
	x_(releases(repo)) $
	xFlatMap(tag := {
		run_benchmarks(repo_path, benchmarks, tag @ name)
	})                 $
	x_Join()





if ( !is_current_version(xVersion()) ) {

	message('-- reinstalling latest version of ', config $ reponame)
	install_github(config $ reponame, config $ username, ref = 'releases')

}





timings_as_dataframe <- timings := {

	x_(timings) $ xGroupBy(x. $ file) $ xMap(xSecondOf %then% xZip) $ xMap(columns := {

		data.frame(
			file   = x_( columns[[1]] ) $ xMap(xExplode('/bench-') %then% xLastOf) $ x_AsCharacter(),
			expr   = xAsCharacter( columns[[2]] ),
			ref    = xAsCharacter( columns[[3]] ),

			lower  = xAsDouble(    columns[[4]] ),
			median = xAsDouble(    columns[[5]] ),
			upper  = xAsDouble(    columns[[6]] ),

			stringsAsFactors = False
		)

	})

}





plot_timings <- timings := {

	wide_dfs <- timings_as_dataframe(timings)

	x_(wide_dfs) $ x_Do(wide_df := {

		gg <-
			ggplot(wide_df) +

			geom_point(
				aes(
					x = reorder(ref, order( as.numeric(gsub('[v]|[.]', '', ref)) )),
					y = median, color = expr, guide = file), alpha = 0.6) +

			geom_errorbar(
				aes(
					x     = reorder(ref, order( as.numeric(gsub('[v]|[.]', '', ref)) )),
					ymin  = lower,
					ymax  = upper,
					color = expr,
					guide = file
				), width = 0.4, alpha = 0.2) +

			xlab("")   +
			ylab("Hz") +
			ggtitle("Kea performance between releases.") +

			scale_y_log10( breaks = 10 ^ (1:6), labels = comma(10 ^ (1:6)) )



		fname <- xAmend('[.][R]$|[.][r]$', '', xFirstOf(wide_df $ file))
		fpath <- xFromChars_('~/Desktop/bench-', fname)
		width <- 100 * xLenOf(releases(repo)) + 200

		png(fpath, res = 150, width = width, height = 1000)
			plot(gg)
		dev.off()

	})

}





plot_timings(timings)
