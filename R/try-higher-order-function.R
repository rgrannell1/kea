
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
				throw_kea_error(sys.call(1), err $ message)
				invisible(NULL)
			},
			warning = function (warn, ...) {
				throw_kea_warning(sys.call(1), warn $ message)
				invisible(NULL)
			}
	))

}
