
# I don't want all caps to frighten timid users

Null <- NULL
True <- TRUE
False <- FALSE

# to complete the following triad in the cleanest way;
# if ( True ); if ( !False ), if( ._(Na) )

._ <- is.na

object <- function () {
	new.env(parent = emptyenv())
}

call_with_params <- function (name, fn) {
	# string -> function -> call
	# create call for a function with
	# the arguments of another function.

	as.call(
		lapply(
			c(name, names(formals(fn)) ),
			as.symbol))
}
