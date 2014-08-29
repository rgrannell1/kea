
# Write a very large number of test cases to a file,
# for use in benchmarking

bench_cases <- function (num, fpath) {

	saveRDS(lapply(1:num, function (ith) {
		from_stream(ith %% 10000)
	}), fpath)

}

# bench_cases(100000, '~/Code/kea.R/inst/benchmarks/bench-cases.rds')
