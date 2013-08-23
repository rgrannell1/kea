
say <- ( function () {

	this <- object()
	this$method_not_found <-
		function (pcall, inputs) {

			readable <- list(
				class =	
					if (is.vector(inputs$x) || is.pairlist(inputs$x)) {
						"collections"
					} else if (is.function (inputs$x)) {
						"functions"
					}
			)

			stop(paste0(
				pcall, '\n',
				"the method ", dQuote(inputs$method), " is not currently implemented for ",
				readable$class
			), call. = False)

		}
	this
} )()
