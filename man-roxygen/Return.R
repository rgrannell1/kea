
#' @section Short-Circuiting:
#'
#' The fold family of functions include an optional mechanism of
#' prematurely returning an answer, similar to the \code{return()} function
#' in base R.
#
#' By calling \code{Return} with the value you want \code{fn} to yield instantly,
#' the fold/reduce is halted, and that value is yielded as the result of
#' that proceedure. This allows fold/reduce to have sub-O(n) efficiency.
#'
#' \code{Return()} essentially acts as a higher-order return function,
#' that breaks "further" than the normal return statement.
#'
#' See 'Examples' for an example of short-circuiting in use.
#'
