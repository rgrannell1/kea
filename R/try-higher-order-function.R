
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
				throw_kiwi_error(sys.call(1), err $ message)
				invisible(Null)
			},
			warning = function (warn, ...) {
				throw_kiwi_warning(sys.call(1), warn $ message)
				invisible(Null)
			}
	))

}
