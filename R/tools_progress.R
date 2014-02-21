
progress_bar <- function (iters, invoking_call) {
	# generate a progress bar that can print
	# a summary of how long is left to run.

	bars <- 0
	fn_call <- invoking_call[[1]]
	start_time <- as.numeric(Sys.time())

	function (iter) {

		# braces slightly speed up check
		if ( (iter/iters) * 100  %/% 5 > bars ) {
			# emit a well-formated text progress bar.

			bars <<- bars + 1

			components <- local({

				this <- Object()

				this$seconds_elapsed_text <-
					gettextf("%8s", as.numeric(Sys.time()) -  start_time)

				this$seconds_remaining <-
					round((this$seconds_elapsed / iter) * iters - this$seconds_elapsed)

				this$percent_complete <-
					round((iter / iters) * 100)

				this$progress_arrow <-
					gettextf(
						'%-20s',
						paste0(
							# an = for each 5%, and a terminal arrow.
							c(rep('=', this$percent_complete %/% 5), '>'),
							collapse = ''))

				this$percent_complete_text <-
					gettextf('%6s', paste0(this$percent_complete, '%'))

				this
			})

			message <- paste0(
				'\r', paste0(fn_call),
				'[',
					components $ percent_complete_text,
					components $ progress_arrow,
				'] ',
				iter, ',', iters,
				components $ seconds_elapsed_text, 's'
			)

			cat(message)
		}
	}
}
