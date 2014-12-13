
#
#
#
#

Try_Higher_Order_Function <- function (EXPR) {

	EXPR <- substitute(EXPR)

	bquote(
		withCallingHandlers(
			.(EXPR),
			error = function (err) {

				if (err $ message == "could not find function \"Return\"") {

					message <- "all calls to Return( ) must be made in the outermost" %+%
						" function used as an input."

					throw_exception $ lookup(sys.call(1), message)

				} else {
					throw_exception $ error(sys.call(1), err $ message)
					invisible(NULL)
				}

			},
			warning = function (warn, ...) {

				throw_exception $ warning(sys.call(1), warn $ message)
				invisible(NULL)

			}

	))

}
