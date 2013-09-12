
# I don't want all caps to frighten timid users

Null <- NULL
Na <- NA
True <- TRUE
False <- FALSE

# to complete the following triad in the cleanest way;
# if ( True ); if ( !False ), if( `_`(Na) )

`_` <- is.na

object <- function () {
	new.env(parent = emptyenv())
}

call_with_params <- function (name, fn) {
	# string -> function -> call
	# create call for a function with
	# the arguments of another function.
	
	as.call(
		lapply(
			c(name, names(xFormals(fn)) ),
			as.symbol))
}

assert <- function (bool, pcall) {
	args <- as.list(match.call())[-1]

	if (!bool) {
		call <- if (missing(pcall)) {
			'assert()'
		} else {
			paste0(deparse(pcall), collapse = '')
		}
		stop(
			call,
			": the assertion\n",
			"    ", paste0(deparse(args$bool), collapse = ''), "\n",
			"failed.",
			call. = False)
	}
}

# not included arbitrarily; I sometimes use this constant.
#' @export

tau <- 6.2831853071795864769252867

# to make error messages clearer.

traits <- list(
	list_of_collections = 
		paste0('list_of_', c('list', 'pairlist', 'vector')),
	list_of_recursive = 
		paste0('list_of_', c('list', 'pairlist')),
	collection_of_string = 
		paste0(c('list_of_', 'vector_of_', 'pairlist_of_'), 'string'),
	collection_of_number = c(
			paste0(c('list_of_', 'vector_of_', 'pairlist_of_'), 'integer'),
			paste0(c('list_of_', 'vector_of_', 'pairlist_of_'), 'double')),
	positive_number = 
		c("positive double", "positive integer"),
	functionable = 
		c('function', 'string', 'symbol'),
	collection = 
		c('list', 'pairlist', 'vector'),
	recursive = 
		c('list', 'pairlist')
)
