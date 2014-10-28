
require(git2r,          quietly = TRUE, warn.conflicts = FALSE)
require(devtools,       quietly = TRUE, warn.conflicts = FALSE)
require(microbenchmark, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2,        quietly = TRUE, warn.conflicts = FALSE)
require(scales,         quietly = TRUE, warn.conflicts = FALSE)
require(reshape2,       quietly = TRUE, warn.conflicts = FALSE)

# -- I questionably use kea to profile kea, so the library is
# -- loaded and unloaded and removed frequently below.

if (!require(kea)) {

	suppressMessages({

		install_github('kea', 'rgrannell1', ref = 'releases')
		require(kea, quietly = TRUE, warn.conflicts = FALSE)

	})

}

stopwatch <- xStopwatch

# get file paths relative to the repositories location.

get_path <- (...) := {
	xImplode(.Platform $ file.sep, xJoin_(getwd(), ...))
}






config <- list(
	username    = 'rgrannell1',
	reponame    = 'kea',
	repo_url    = "https://github.com/rgrannell1/kea",

	benchmarks  = get_path("inst/benchmarks"),
	total_time  = NA, # todo; evenly divide maxtime between expressions.
	seconds     = 0.5
)





# if the latest version of kea isn't installed, go to
# github and reinstall it.

reinstall_current_kea <-local({

	is_latest_kea_version <- xIs(xVersion())

	function () {
		if ( !is_latest_kea_version(xVersion()) ) {

			install_github('kea', 'rgrannell1', ref = 'releases')
			require(kea, quietly = TRUE, warn.conflicts = FALSE)

		}
	}
})

on.exit(reinstall_current_kea())



# create a temporary folder to work within.

setup_path <- function (debug) {
	if (debug) {
		'/tmp/r-benchmark'
	} else {
		tempfile(pattern = "r-benchmark-")
	}
}

# download the git repository if it isn't already downloaded.

setup_repo <- function (repo_path) {

	if (length(list.files(repo_path)) == 0) {
		clone(config $ repo_url, repo_path)
	} else {
		repository(repo_path)
	}

}




repo_path <- setup_path(True)
repo      <- setup_repo(repo_path)



# list every tag matching the form v[x].[y].[z], sorted
# properly.

releases <- repo := {

	is_release <- '^v[0-9]+[.][0-9]+[.][0-9]+$'

	x_(tags(repo)) $
	xSelect(tag := {
		xIsMatch(is_release, tag @ name)
	})             $
	xSortBy(tag := {
		as.numeric(xAmend('v|[.]', '', tag @ name))
	})             $
	x_Take(10)
}



# load the current benchmarks as filename, file content pairs.

benchmarks <-
	x_(list.files(config $ benchmarks, full.names = True)) $
	x_Map(
		xJuxtapose_(xI, parse))



# load a particular version of the repository
# an execute a callback after loading that package in a seperate environment.

try_load <- (ref : callback) := {

	test_env <- new.env(parent = environment())

	tryCatch(
		eval({

			checkout(ref, force = True)
			suppressMessages(install.packages(repo_path, repos = Null, type = "source"))

			callback( )

		}, test_env),
		error = err := {
			message('\n--- failure while loading ', ref @ name)
		},
		warning = warn := {
			message('\n--- warning while loading ', ref @ name)
		}
	)

}





try_benchmark <- (benchmarks : ref : seconds) := {

	message( "\n--- benchmarking ", ref @ name, '\n' )

	lapply(seq_along(benchmarks), function (ith) {

		benchmark        <- benchmarks[[ith]]

		benchmark_file   <- benchmark[[1]]
		benchmarks_exprs <- benchmark[[2]]

		lapply(seq_along(benchmarks_exprs), function (jth) {

			test_env <- new.env(parent = environment())

			expr          <- benchmarks_exprs[[jth]]
			deparsed_expr <- paste0(deparse(expr), collapse = '\n')

			expr_times <- tryCatch(
				eval(bquote({

					expr_times     <- list()
					time_remaining <- stopwatch(seconds)

					while (time_remaining()) {

						# cargo-cult programming.
						gc(verbose = FALSE)

						group_times <- microbenchmark(
							.(expr), unit = 'hz', times = 60, control = list(warmup = 10)) $ time

						expr_times <- c(expr_times, group_times)
					}

					range <- summary(1 / as.numeric(expr_times) * 10^9)

					list(
						file   = benchmark_file,
						expr   = deparsed_expr,
						ref    = ref @ name,

						lower  = range[['1st Qu.']],
						median = range[['Median']],
						upper  = range[['3rd Qu.']]
					)

				}), envir = test_env),

				error = function (err) {

					list(
						file   = benchmark_file,
						expr   = deparsed_expr,
						ref    = ref @ name,

						lower  = 0,
						median = 0,
						upper  = 0
					)

				},
				warning = function (warn) {

					list(
						file   = benchmark_file,
						expr   = deparsed_expr,
						ref    = ref @ name,

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


# load a version of package and benchmark that code.

run_benchmarks <- (repo_path : benchmarks: ref) := {
	try_load(ref, function () {
		try_benchmark(benchmarks, ref, config $ seconds)
	})
}

# benchmark every version of a repository.

benchmark_each_version <- (repo : benchmarks : repo_path) := {

	x_(releases(repo)) $
	xFlatMap(tag := {
		run_benchmarks(repo_path, benchmarks, tag)
	})                 $
	x_Join()

}

timings <- benchmark_each_version(repo, benchmarks, repo_path)
reinstall_current_kea()





# convert each set of timing data to a data frame for plotting.

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

		file_plot <-
			ggplot(wide_df) +

			geom_point(
				aes(
					x = reorder(ref, order( as.numeric(gsub('[v]|[.]', '', ref)) )),
					y = median, color = expr, guide = file), alpha = 0.8) +

			geom_errorbar(
				aes(
					x     = reorder(ref, order( as.numeric(gsub('[v]|[.]', '', ref)) )),
					ymin  = lower,
					ymax  = upper,
					color = expr,
					guide = file
				), width = 0.4, alpha = 0.4) +

			xlab("")   +
			ylab("Hz") +
			ggtitle("Kea performance between releases.") +

			scale_y_log10( breaks = 10 ^ (1:6), labels = comma(10 ^ (1:6)), limits = c(10^1, 10^6) )

		print(wide_df)



		fname      <- xAmend('[.][R]$|[.][r]$', '', xFirstOf(wide_df $ file))
		fpath      <- xFromChars_('~/Desktop/benchmark/bench-', fname)

		plot_width <- 100 * xLenOf(releases(repo)) + 500

		message('-- saving benchmark plot to ', fpath, '.\n')

		png(fpath, res = 150, width = plot_width, height = 1000)
			plot(file_plot)
		dev.off()

	})

}





plot_timings(timings)
