
# I don't want all caps to frighten timid users

Null <- NULL
True <- TRUE
False <- FALSE

# to complete the following triad in the cleanest way;
# if ( True ); if ( !False ), if( ._(Unknown) )
# hopefully bquote() doesn't get sick over my code.

._ <- is.na

object <- function () {
	new.env(parent = emptyenv())
}
