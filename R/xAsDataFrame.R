
#' xAsDataFrame
#'
#' Convert a collection of collections to a data.frame
#'
#' @param
#'    colls a collection of collections.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    Returns a data.frame.
#'
#' @details
#'    data.frames are not a beloved part of arrow,
#'    but they are ubiquitous in the base language.
#'    Arrow uses collections of collections instead of
#'    data.frames, but data.frames are more common in the
#'    base language and importantly in libraries like ggplot2.
#'
#' @section Corner Cases:
#'    \bold{xAsDataFrame} tried hard to avoid the crazy corner
#'    cases of \bold{data.frame}, but data frames are kludgy and
#'    there's only so much they can be made workable.
#'
#'    Character vectors are not converted into factors,
#'    unlike the base functions.
#'
#'   Rownames are added to the data.frame if the names of the
#'   collections inside colls are identical.
#'   If the rownames are different, an error is thrown.
#'
#'   If row names or column names are ommitted they default to
#'   indices.
#'
#' @export

xAsDataFrame <- function (colls) {
	# Collection Collection any -> data.frame any
	# convert a collection to a data frame.

	invoking_call <- sys.call()

	assert(
		!missing(colls), invoking_call,
		exclaim$parametre_missing(colls))

	insist$must_be_collection(colls, invoking_call)
	insist$must_be_collection_of_collections(colls, invoking_call)
	insist$must_be_collection_of_equal_length(colls, invoking_call)

	if (length(colls) == 0) {
		unname(as.data.frame(matrix(nrow = 0, ncol = 0 )) )
	} else {

		insist$must_be_collection_of_equal_names(
			colls, invoking_call)

		colls_colnames <- if ( !is.null(names(colls)) ) {
			names(colls)
		} else {
			seq_along(colls)
		}

		colls_rownames <- if (!is.null( names( colls[[1]] ) )) {
			names( colls[[1]] )
		} else {
			seq_along( colls[[1]] )
		}

		# use I() to allow a list column, no string coercioun.
		colls <- lapply(colls, I)

		df <- do.call(data.frame, colls)

		colnames(df) <- colls_colnames
		rownames(df) <- colls_rownames
		df
	}
}

#' @rdname xAsDataFrame
#' @export

xAsDataFrame... <- function (...) {
	xAsDataFrame(list(...))
}
