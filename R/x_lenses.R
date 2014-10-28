#
## The identity lens
##
#
#y_ <- MakeFun(function (val) {
#
#	out <- function () {
#		list(
#			get = function () val,
#			set = function (val) val
#		)
#	}
#
#	class(out) <- 'y_'
#	out
#
#})
#
#
#
#
#
#`$.y_` <- function (outer, inner) {
#
#	inner_name <- paste0(substitute(inner))
#	proto       <- get_proto_ref( obj[['x']] )
#
#	if ( !any(proto[[2]] == inner_name) || inner_name == "private" ) {
#		# -- the invoked inner wasn't found, so we should give a suggestion.
#
#		# -- required for proper call formatting in output.
#		invoking_call <- call('$', sys.call()[[2]], sys.call()[[3]] )
#
#		contents_are <- proto[[1]][['private']][['contents_are']]
#
#		suggest_similar_inner(
#			obj[['x']], inner_name, contents_are, invoking_call)
#
#	}
#
#	fn <- proto[[1]][[inner_name]]
#
#	print(fn)
#
#	lcompose(outer, inner)
#
#}
#
#
#
#
#
#lcompose <- '%andThen%' <- function (outer, inner) {
#
#	inner <- eval(body(inner))
#	outer <- eval(body(outer))
#
#	function () {
#
#		list(
#			get = function (target) {
#				inner $ get(outer $ get(target))
#			},
#			set = function (target, replacement) {
#				outer $ set(target, inner $ set(outer $ get(target), replacement))
#			}
#		)
#
#	}
#
#}
#
#functionLens <-
#	function (fn) {
#		function () {
#			list(
#				get = identity,
#				set = identity
#			)
#		}
#	}
#
#
#firstOf <- function () {
#
#	list(
#		get = function (coll) coll[[1]],
#		set = function (coll, val) {
#			coll[[1]] = val
#			coll
#		}
#	)
#
#}
#
#secondOf <- function () {
#	list(
#		get = function (coll) coll[[2]],
#		set = function (coll, val) {
#			coll[[2]] = val
#			coll
#		}
#	)
#}
#
#
#
#lens <- firstOf %andThen% secondOf %andThen% functionLens(paste)
#
#
#
#
#
#coll <- list(list(1, 2, 3), 4, 5)
##lens $ get(coll)
