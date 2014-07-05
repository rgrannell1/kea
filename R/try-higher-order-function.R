
#
#
#
#

Try_Higher_Order_Function <- function (EXPR) {

	EXPR <- substitute(EXPR)

	bquote(tryCatch(
		.(EXPR),
		error = function (err) {
			throw_kiwi_error(sys.call(1), err $ message)
		},
		warning = function (warn) {
			throw_kiwi_warning(sys.call(1), warn $ message)
		}
	))

}
