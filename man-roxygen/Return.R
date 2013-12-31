
#' @section Short-Circuiting:
#'
#' The fold family of functions include an optional mechanism of
#' prematurely returning an answer, similar to the \code{return()} function
#' in base R.
#
#' By calling \code{Return} with the value you want \code{fn} to yield instantly,
#' the fold/reduce is halted, and that value is yielded as the result of
#' that proceedure.
#'
#' \code{Return()} essentially acts as a higher-order return function,
#' that breaks "further" than the normal return statement.
#'
#' \code{to_search <- c(list(0, 1, 0, 3, 2, 10), rep(4, 1000))}
#'
#' \code{
#' xFoldl(\cr
#'     (checked : searched ) := {\cr
#'         if (searched == 10) Return(checked) else checked + 1\cr
#'     },\cr
#'     list(),\cr
#'     to_search\cr
#' )
#' }
#'
#' \code{6}
#'
#' In this case, the Foldl returns its results prematurely after 6 searches,
#' as opposed to the thousands of iterations that the base function \code{Reduce()} would
#' have to preform before returning its result. By using \code{Return()} where possible
#' folding functions can be made to preform significantly bettern
#'
