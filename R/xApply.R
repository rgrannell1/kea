
#' xApply
#'
#' Invoke a function with a collection of arguments.
#'
#' @section Type Signature:
#'     function -> |any| -> any
#'
#' @details
#'    \bold{xApply} is an adapter function that allows any function
#'    to be called with a collection of arguments. A typical use case
#'    is to allow a function with ellipsis (...) arguments to be called with
#'    a variable number of arguments.
#'
#'    \code{coll <- list( list(1, 2), list(3, 4), list(5, 6) )}
#'
#'    \code{xApply(rbind, coll)}
#'
#'    In the above case the function rbind is called on several rows, without
#'    having to use reduce or a for loop to successively bind the rows.
#'
#'    It isn't usually necessary to call kea functions with \bold{xApply},
#'    since they exist with both variadic and non-variadic forms.
#'
#' @param
#'    fn an function of any arity. The function to call.
#'
#' @param
#'    coll a collection. The arguments to pass to
#'    \bold{fn}. The collection may be named
#'    or unnamed; normal R function call semantics
#'    dictate how the arguments are used by the input function.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    The return value of \bold{fn}.
#'
#' @section Corner Cases:
#'     Fails when too many arguments are given to \bold{fn}.
#'
#' @family function_application_functions
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xApply.R
#'
#' @rdname xApply
#' @export

xApply <- MakeFun('xApply', function (fn, coll) {

	 MACRO( Try_Higher_Order_Function( eval(
		as.call(c(fn, coll)),
		envir = parent.frame()) ) )
})

#' @rdname xApply
#' @export

xApply_ <- MakeVariadic(xApply, 'coll')
