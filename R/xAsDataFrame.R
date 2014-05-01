
#' xAsDataFrame
#'
#' Convert a collection of collections to a data.frame
#'
#' @param
#'    colls a collection of collections. The collection
#'    to convert to a data frame.
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

xAsDataFrame <- MakeFun(function (colls) {

	MACRO( Must $ Not_Be_Missing(colls) )
	MACRO( Must $ Be_Collection(colls) )

	MACRO( Must $ Be_Collection_Of_Collections(colls) )
	MACRO( Must $ Be_Collection_Of_Equal_Length(colls) )


	if (length(colls) == 0) {
		unname(as.data.frame(matrix(nrow = 0, ncol = 0 )) )
	} else {

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

		# use I() to allow a list column, no string coercion.
		colls <- lapply(colls, I)

		df <- do.call(data.frame, colls)

		colnames(df) <- colls_colnames
		rownames(df) <- colls_rownames
		df
	}
})

#' @rdname xAsDataFrame
#' @export

xAsDataFrame_ <- MakeVariadic(xAsDataFrame, 'colls')
