
# The identity lens
#

y_ <- MakeFun(function (val) {

	out <- list(x = val)

	out $ yget     <- function () y_(out $ x)
	out $ y_get    <- function () out $ x

	out $ yset     <- function (val2) y_(out $ x)
	out $ y_set    <- function (val2) out $ x

	out $ ymap     <- function (fn) {
		y()
	}
	out $ y_map    <- function (fn) {
		out $ yset( fn(out $ yget()) )
	}

	out $ y_unlens <- function () {
		out $ x
	}

	class(out) <- 'y_'
	out

})





`$.y_` <- function (lens, method) {


	method_name <- paste0(substitute(method))
	proto       <- get_proto_ref( lens	[['x']] )

	if ( !any(proto[[2]] == method_name) || method_name == "private" ) {
		# -- the invoked method wasn't found, so we should give a suggestion.

		# -- required for proper call formatting in output.
		invoking_call <- call('$', sys.call()[[2]], sys.call()[[3]] )

		contents_are <- proto[[1]][['private']][['contents_are']]

		suggest_similar_method(
			lens	[['x']], method_name, contents_are, invoking_call)

	}

	fn <- proto[[1]][[method_name]]

	environment(fn)[['Self']] <- function () {
		lens[['x']]
	}

	fn
}
