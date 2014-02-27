
#' $ xTap
#'
#' Apply an anonymous function to the contents of an arrow object.
#'
#' @details
#'    \bold{xTap} applies anonymous methods to arrow objects. This
#'    serves the same purpose as anonymous functions in the base language.
#'
#'    \code{x_(letters) $ xTap(xs := length(xs) == 26)}
#'
#'    \bold{xTap} also allows useful base functions to be used by
#'    arrow objects.
#'
#' @usage
#'      x_(  ) $ xTap(fn)
#'
#' @return
#'      An arrow object.
#'
#' @family method_functions
#'
#' @name xTap

NULL
