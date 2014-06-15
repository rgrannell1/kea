
#' xJuxtapose
#'
#' Create a function that applies an argument to a list of underlying functions.
#'
#' @section Type Signature:
#'     |function| -> (..any -> [any])
#'
#' @details
#'    \bold{xJuxtapose} is a method of applying one value to several functions
#'    simultaneously. The function returned by \bold{xJuxtapose} returns
#'    a list - on element for each input function to \bold{xJuxtapose}.
#'    Each element of this list contains the result of calling an element of
#'    \bold{fns} with the arguments supplied to \bold{xJuxtapose}'s return function.
#'
#'    A function to summarise a data set can be implemented with \bold{xJuxtapose}:
#'
#'    \code{summarise <- xJuxtapose_(min, median, max)}
#'
#'    \code{summarise(c(3, 1, 2, 4))}
#'
#'    \code{list(1, 2.5, 4)}
#'
#'    Each function passed to \bold{xJuxtapose} is invoked with the data set,
#'    and the results are collected in a list.
#'
#' @param
#'    fns a collections of functions. The functions
#'    to call with a set of arguments.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    A variadic function that returns a list.
#'
#' @section Corner Cases:
#'    If no functions are provided the empty list is returned.
#'
#' @family function_modifying_functions
#'
#' @family function_combining_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xJuxtapose.R
#'
#' @rdname xJuxtapose
#' @export

xJuxtapose <- MakeFun(function (fns) {

	if (length(fns) == 0) {
		list()
	} else {

		function (...) {
			"a function created by xJuxtapose."
			""
			lapply(fns, function (fn) fn(...))
		}
	}
})

#' @rdname xJuxtapose
#' @export

xJuxtapose_ <- MakeVariadic(xJuxtapose, 'fns')
