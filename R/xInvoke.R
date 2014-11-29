
#' xInvoke
#'
#' Invoke a unary collection with a collection of arguments.
#'
#' @section Type Signature:
#'     (|any| -> any) -> |any| -> any
#'
#' @details
#'     \bold{xInvoke} is an adaptor function somewhat like \bold{xApply}; in
#'     fact it was added to kea almost solely because the type signature of \bold{xApply}
#'     implies that this function is necessary and probably useful.
#'
#'     \bold{xInvoke} allows a function that takes a single arguments list to be
#'     called with that list of parametres, or be called with a variable number of
#'     arguments if \bold{xInvoke_} is used.
#'
#'    It isn't usually necessary to call kea functions with \bold{xInvoke},
#'    since they exist with both variadic and non-variadic forms.
#'
#' @param
#'    fn a unary function. The function to call.
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
#'    inst/examples/example-xInvoke.R
#'
#' @rdname xInvoke
#' @export

xInvoke <- MakeFun(function (fn, coll) {

	MACRO( Must_Have_Arity(fn, 1) )

	MACRO( Try_Higher_Order_Function( fn(coll) ) )

})

#' @rdname xInvoke
#' @export

xInvoke_ <- MakeVariadic(xInvoke, 'coll')
