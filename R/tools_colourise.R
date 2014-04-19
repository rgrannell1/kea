colourise <- local({
	# functions that add ANSI colour codes to strings, allowing them to
	# be colourised.

	supports_colour <- function () {
		# is a terminal colourisable?

		env_vars <- Sys.getenv()

		TERM      <- env_vars["TERM"]
		COLORTERM <- env_vars["COLORTERM"]

		set_env_vars <- names(env_vars)

		# term support color.
		matching_TERM <-
			("TERM" %in% set_env_vars) && !is.na(TERM) && TERM %in%
			c("screen", "screen-256color", "xterm-color", "xterm-256color")

		# colorterm is set at all.
		matching_COLORTERM <-
			("COLORTERM" %in% set_env_vars) && !is.na(COLORTERM)

		matching_TERM || matching_COLORTERM
	}

	list(
		black =
			function (message) {
				if (supports_colour()) {
					"\033[0;30m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		blue =
			function (message) {
				if (supports_colour()) {
					"\033[0;34m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		green =
			function (message) {
				if (supports_colour()) {
					"\033[0;32m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},
		red =
			function (message) {
				if (supports_colour()) {
					"\033[0;31m" %+% message %+% "\033[0m"
				} else {
					message
				}
			},

		yellow =
			function (message) {
				if (supports_colour()) {
					"\033[1;33m" %+% message %+% "\033[0m"
				} else {
					message
				}
			}
	)
})
