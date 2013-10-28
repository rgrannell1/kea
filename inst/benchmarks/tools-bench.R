

require(simpleboot)

delayedAssign('mb', microbenchmark)

stats <- list(
	percent_mad =
		function (data) {
			100 * (mad(data) / median(data))
		},
	median_diff_ci =
		function (independent, control) {

			require(boot)

			sampleset <- if (length(data) > 1000) {
				sample(data, size = 1000)
			} else {
				data
			}

			boot_data <- boot.ci(
				two.boot(independent, control, median, R = 100000),
				conf = 0.95, type = 'bca')

			list(
				lower =
					boot_data$bca[4] / median(control),
				upper =
					boot_data$bca[5] / median(control))
		}
)

tprofile <- function (info = '', independent, control, max_time = 1) {
	# profile the performance for several functions

	require(microbenchmark)

	message('--- ', info)

	warmup_data <- mb(independent(), times = 10)$time
	median_seconds <- median(warmup_data) / 10^9
	iters <- floor(max_time / median_seconds)

	report <- list(
		independent = list(
			data = mb(independent(), times = iters)$time),
		control = list(
			data = mb(control(), times = iters)$time)
	)

	report$independent <- c(
		report$independent, list(
			median =
				median(report$independent$data),
			percent_mad =
				stats$percent_mad(report$independent$data),
			iters = iters) )

	report$control <- c(report$control,
		list(
			median =
				median(report$control$data),
			percent_mad =
				stats$percent_mad(report$control$data),
			iters = iters) )

	report$difference <-
		stats$median_diff_ci(
			report$independent$data,
			report$control$data)

	report
}

tprofile(
	independent = function () Sys.sleep(0.1),
	control = function () Sys.sleep(0.03))