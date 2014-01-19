
#' Special Values
#'
#' Arrow includes Pascal-case locked-versions of R's uppercase special values.
#'
#' @details
#'    These values are pascal-case varients of na,
#'    null, true and false. These values cannot be
#'    overwritten, like the uppercase varients but
#'    unlike the variables \code{T} and \code{F}.

#' @export
#' @rdname values

Na <- NA
# lockBinding('Na', environment())

#' @export
#' @rdname values

Null <- NULL
# lockBinding('Null', environment())

#' @export
#' @rdname values

True <- TRUE
# lockBinding('True', environment())

#' @export
#' @rdname values

False <- FALSE
# lockBinding('False', environment())
