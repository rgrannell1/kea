
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
	username   = 'rgrannell1',
	reponame   = 'kea',

	repo_url   = "https://github.com/rgrannell1/kea",
	graph_path = "~/Desktop/benchmark",

	benchmarks = get_path("inst/benchmarks"),
	total_time = .35 * 60,
	limit      = Inf
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

		fallback <- function (cond) {

			message("-- falling back to shell cloning.")

			system(paste0("git clone ", config $ repo_url, ".git ", repo_path))
			repository(repo_path)

		}

		tryCatch(
			clone(config $ repo_url, repo_path),
			error = fallback
		)


	} else {
		repository(repo_path)
	}

}




repo_path <- setup_path(True)
repo      <- setup_repo(repo_path)



# list every tag matching the form v[x].[y].[z], sorted
# properly.

releases <- (repo : use_head) := {

	is_release <- '^v[0-9]+[.][0-9]+[.][0-9]+$'

	x_(tags(repo))        $
	xSelect(tag := {
		xIsMatch(is_release, tag @ name)
	})                    $
	xSortBy(tag := {
		as.numeric(xAmend('v|[.]', '', tag @ name))
	})                    $
	xReverse()            $
	xTake(config $ limit) $
	xAppend(head(repo))   $
	x_Reverse()
}



# load the current benchmarks as filename, file content pairs.

benchmarks <-
	x_(list.files(config $ benchmarks, full.names = True)) $
	x_Map(
		xJuxtapose_(xI, parse))

number_of_releases <-
	xLenOf(releases(repo, True))

number_of_benchmarks <-
	x_(benchmarks)                              $
	xMap(
		xSecondOf %then% as.list %then% xLenOf) $
	x_Reduce(`+`)




run_time       <- config $ total_time / (number_of_releases * number_of_benchmarks)

message("-- running for ", run_time, " seconds \n.")



justTry <- function (expr) {
	tryCatch(eval(expr), error = function (err) {}, warning = function (warn) {})
}





# load a particular versionf of the repository
# an execute a callback after loading that package in a seperate environment.

try_load <- (ref : callback) := {

	refname <- tryCatch(
		ref @ name,
		error = function (err) {"HEAD"}
	)

	test_env <- new.env(parent = environment())
	test_env $ refname <- refname

	tryCatch(
		eval({

			checkout(ref, force = True)
			suppressMessages(install.packages(repo_path, repos = Null, type = "source"))

			package_number <- as.numeric(gsub('[v]|[.]', '', refname))

			if (package_number < 160) {

				justTry(detach('package:arrow', unload = TRUE))
				justTry(detach('package:kiwi',  unload = TRUE))
				justTry(detach('package:kea',   unload = TRUE))

				require(arrow, quietly = TRUE, warn.conflicts = FALSE)
				message('loaded ', packageVersion('arrow'))

			} else if (package_number < 420) {

				justTry(detach('package:arrow', unload = TRUE))
				justTry(detach('package:kiwi',  unload = TRUE))
				justTry(detach('package:kea',   unload = TRUE))

				require(kiwi, quietly = TRUE, warn.conflicts = FALSE)
				message('loaded ', packageVersion('kiwi'))

			} else {

				justTry(detach('package:arrow', unload = TRUE))
				justTry(detach('package:kiwi',  unload = TRUE))
				justTry(detach('package:kea',   unload = TRUE))

				require(kea, quietly = TRUE, warn.conflicts = FALSE)
				message('loaded ', packageVersion('kea'))

			}

			callback( )

		}, test_env),
		error = err := {
			message('\n--- failure while loading ', refname)
			message(err $ message)
		},
		warning = warn := {
			message('\n--- warning while loading ', refname)
			message(warn $ message)
		}
	)

}





timing_result <- function (file, expr, ref, lower = 0, median = 0, upper = 0) {
	list(file = file, expr = expr, ref = ref, lower = lower, median = median, upper = upper)
}

try_benchmark <- (benchmarks : ref : total_time) := {

	refname <- tryCatch(
		ref @ name,
		error = function (err) {"HEAD"}
	)

	message( "\n--- benchmarking ", refname, '\n' )

	lapply(seq_along(benchmarks), function (ith) {

		benchmark        <- benchmarks[[ith]]

		benchmark_file   <- benchmark[[1]]
		benchmarks_exprs <- as.list( benchmark[[2]] )

		setup_expr <- if (length(benchmarks_exprs) > 0) {

			out              <- eval( benchmarks_exprs[[1]] )
			benchmarks_exprs <- xRestOf(benchmarks_exprs)
			out

		} else {
			list()
		}

		message('------ benchmarking ', benchmark_file)

		lapply(seq_along(benchmarks_exprs), function (jth) {

			test_env <- new.env(parent = environment())
			test_env $ refname <- tryCatch(
				ref @ name,
				error = function (err) {"HEAD"}
			)

			for (variable in names(setup_expr)) {
				test_env[[ variable ]] <- setup_expr[[variable]]
			}

			expr          <- benchmarks_exprs[[jth]]
			deparsed_expr <- paste0(deparse(expr), collapse = '\n')

			expr_times <- tryCatch(
				eval(bquote({

					expr_times     <- list()
					time_remaining <- stopwatch(run_time)

					while (time_remaining()) {

						# cargo-cult programming.
						gc(verbose = FALSE)

						group_times <- microbenchmark(
							.(expr), unit = 'hz', times = 30, control = list(warmup = 10)) $ time

						expr_times <- c(expr_times, group_times)
					}

					quartiles <- summary(1 / as.numeric(expr_times) * 10^9)

					timing_result(
						benchmark_file, deparsed_expr, refname,
						quartiles[['1st Qu.']], quartiles[['Median']], quartiles[['3rd Qu.']]
					)

				}), envir = test_env),

				error = function (err) {
					timing_result(benchmark_file, deparsed_expr, refname)
				},
				warning = function (warn) {
					timing_result(benchmark_file, deparsed_expr, refname)
				}
			)

			expr_times

		})
	})

}


# load a version of package and benchmark that code.

run_benchmarks <- (repo_path : benchmarks : ref) := {
	try_load(ref, function () {
		try_benchmark(benchmarks, ref, config $ total_time)
	})
}

# benchmark every version of a repository.

benchmark_each_version <- (repo : benchmarks : repo_path) := {

	x_(releases(repo, True)) $
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




dpath <- xImplode_(.Platform $ file.sep, config $ graph_path, paste( as.numeric(Sys.time()) ))
dir.create(dpath)






plot_timings <- timings := {

	wide_dfs <- timings_as_dataframe(timings)

	x_(wide_dfs) $ x_Do(wide_df := {

		fname     <- xAmend('[.][R]$|[.][r]$', '', xFirstOf(wide_df $ file))
		refs      <-
		file_plot <-
			ggplot(wide_df) +

			geom_pointrange(
				position  = position_jitter(width = 0.1, height = 0),
				aes(
					x     = reorder(ref, order( as.numeric(gsub('[v]|[.]', '', ref)) )),
					ymin  = lower,
					y     = median,
					ymax  = upper,

					color = expr
				), width  = 0.4, alpha = 0.8) +

			xlab("")   +
			ylab("Hz") +
			ggtitle(paste(fname, "performance between releases.")) +

			scale_y_log10( breaks = 10 ^ (0:7), labels = comma(10 ^ (0:7)), limits = c(10^0, 10^7))
			guides(fill = guide_legend(title = "Expression"))

		fpath      <- xImplode_(.Platform $ file.sep, dpath, xFromChars_(fname, '.png'))
		plot_width <- 80 * xLenOf(releases(repo, True)) + 500






		message('-- saving benchmark plot to ', fpath, '.\n')

		png(fpath, res = 150, width = plot_width, height = 1000)
			plot(file_plot)
		dev.off()

		warnings()
	})

}





plot_timings(timings)
