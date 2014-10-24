
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
				throw_exception $ error(sys.call(1), err $ message)
				invisible(NULL)
			},
			warning = function (warn, ...) {
				throw_exception $ warning(sys.call(1), warn $ message)
				invisible(NULL)
			}
	))

}
