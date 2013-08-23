
# I don't want all caps to frighten timid users

Null <- NULL
True <- TRUE
False <- FALSE
Na <- Unknown <- NA

# to complete the following triad in the cleanest way;
# if ( True ); if ( !False ), if( .(Unknown) )
# hopefully bquote() doesn't get sick over my code.

. <- is.na

object <- function () {
	new.env(parent = emptyenv())
}
