colourise <- local({
	# functions that add ANSI colour codes to strings, allowing them to
	# be colourised.

	supports_colour <- function () {
		# is a terminal colourisable?

		env_vars <- Sys.getenv()

		TERM      <- env_vars["TERM"]
		COLORTERM <- env_vars["COLORTERM"]

		set_env_vars <- names(env_vars)

		# -- term support color.
		matching_TERM <-
			("TERM" %in% set_env_vars) && !is.na(TERM) && TERM %in%
			c("screen", "screen-256color", "xterm-color", "xterm-256color")

		# -- colorterm is set at all. This is required for gnome-terminal.
		matching_COLORTERM <-
			("COLORTERM" %in% set_env_vars) && !is.na(COLORTERM)

		isTRUE(matching_TERM || matching_COLORTERM)
	}

	colouriser <- function (code) {
		function (message) {
			if (supports_colour()) {
				"\033[" %+% code %+% message %+% "\033[0m"
			} else {
				message
			}
		}
	}

	list(
		black =
			colouriser("0;30m"),
		blue =
			colouriser("0;34m"),
		green =
			colouriser("0;32m"),
		red =
			colouriser("0;31m"),
		yellow =
			colouriser("1;33m")
	)
})
