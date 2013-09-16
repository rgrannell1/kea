
bench <- list()

if (Sys.info()["user"] == "rgrannell1") {
	# avoid running on CRAN

	require(microbenchmark)
	collated_timings <- list()

	suffix_regex <- "^bench[-][^-]+[^.]+[.][rR]"

	path <- "/home/rgrannell1/Dropbox/R_directory/arrow-clean/inst/benchmarks"
	
	all_tests <- paste0(path, "/", list.files(
		path, 
		suffix_regex
	))

	source(paste0(path, "/tools-benchmarks.R"))
	sapply(all_tests, source)

	for ( test in sample(names(bench)) ) {

		timings <- 
			benchmark_code(
				bench[[test]], 
				bench[[test]][[1]]$N, 
				bench[[test]][[1]]$times)

		collated_timings[[test]] <- timings

		visualise_benchmark(timings)
	}
	invisible( collated_timings )
}
