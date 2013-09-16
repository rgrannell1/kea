
benchmark_code <- function (tests, len = 100, times = 2) {

	report_result <- function (name, multiplier) {
		# echo the results to the console.	

		multiplier <- round(multiplier, 2)	

		message(
			name, " was ",
			paste(multiplier[1], "+-", multiplier[2]),
			" times slower than the control test")
	}

	compare_results <- function (test, control) {
		# calculate how many times larger x is than y,
		# within a 95% confidence interval
		
		difference <-  median(test) / median(control)
		margin <- abs(
			( median(test) - 2*sd(test) / median(control) + 2*sd(control) ) /
			( median(test) + 2*sd(test) / median(control) - 2*sd(control) ))
			
		c(difference, margin)
	}
	
	timing <- Map(
		function (test) {
			
			cat("..")
			
			raw <- tryCatch(
				list(
					name = test$name,
					test = microbenchmark(
						test$test( len ),
						times = times)$time,
					control = microbenchmark(
						test$control( len ),
						times = times)$time),
				error = function (error) {
					stop(test$name, ":", error)
				}
			)

			# remove n / 20 of the most deviant values, 
			# since microbenchmark generates some extreme outliers
			# initially.

			remove_outliers <- function (timing) {
				
				if (length(timing) < 30) {
					timing
				} else {
					sort_indices <- order( abs(timing - sd(timing)) )

				    cut_of <- ceiling(length(timing) - length(timing) / 20)
					timing[sort_indices < cut_of]					
				}
			}

			list(
				name = raw$name,
				test = remove_outliers( raw$test ),
				control = remove_outliers( raw$control ))

		},
		tests)

	cat("\n")
	
	Map(
		function (test) {
			report_result(
				name = test$name,
				multiplier = compare_results(test$test, test$control))
		},
		timing)
	
	invisible(timing)
}
	
visualise_benchmark <- function (data) {
	# plot the distribution of results for the control
	# and test times, to get an idea of how efficient the 
	# new function is

	require(reshape2)
	require(ggplot2)

	molten <- structure(Map(
		function(x) {
			melt(data.frame(
				name = x$name,
				test = x$test,
				control = x$control), id.vars = "name")
		},
		data),
		names = sapply(data, function (li) li$name))
	
	for (i in seq_along(molten)) {

		g <- ggplot(data = molten[[i]], aes(x = value / 1000)) + 
			geom_line(aes(group = variable, color = variable), stat="density",
				alpha = 0.5, size = 1.5) + 
			xlab("microseconds") + ggtitle(names(molten[i]))

		plot(g)
		Sys.sleep(6)
	}
}
