
#' xStopwatch
#'
#' Create a function that returns true for a preset time after creation.
#'
#' @section Type Signature:
#'    |number| -> (...any -> <logical>)
#'
#' @details
#'   \bold{xStopwatch} lets a program measure the passage of time. For example,
#'    you might wish to stop a program after a set amount of time, or retry a URL
#'   until you loose patience.
#'
#' @param
#'    num a positive number. The number of seconds
#'    the function should return true for.
#'
#' @return
#'    A variadic function that ignores its arguments.
#'
#' @section Corner Cases:
#'    If num is infinite the created timer function will never return false.
#'
#' @family time_functions
#'
#' @family inpure_functions
#'
#' @example
#'    inst/examples/example-xStopwatch.R
#'
#' @template S-Uncertain
#' @rdname xStopwatch
#' @export

xStopwatch <- MakeFun(function (num) {

	MACRO( Must_Be_Longer_Than(0, num) )
	MACRO( Must_Be_Orderable(num) )
	MACRO( Must_Be_Between(num, 0, Inf))

	num     <- unname(num)
	genesis <- Sys.time()

	function (...) {
		"a function returned by xStopwatch."
		""
		difftime(Sys.time(), genesis, units = 'secs') < num
	}
})
