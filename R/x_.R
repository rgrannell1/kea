
x_coll_proto <- local({

	this <- object()
	this$x <- 
		function () {
			thunk_()
		} 
	this$xAll <- 
		function (fn) {
			x_( xAll(fn, thunk_()) )
		}
	this$xAny <- 
		function (fn) {
			x_( xAny(fn, thunk_()) )
		}
	this$xApply <-
		function (fn) {
			x_( xApply(fn, thunk_()) )
		}
	this$xChars <-
		function () {
			x_( xChars(thunk_()) )
		}

	this
})

x_fn_proto <- local({

	this <- object()
	this$x <- 
		function () {
			thunk_()
		} 
	this$xAll <- 
		function (coll) {
			x_( xAll(thunk_(), coll) )
		}
	this$xAny <- 
		function (coll) {
			x_( xAny(thunk_(), coll) )
		}
	this$xApply <-
		function (coll) {
			x_( xApply(thunk_(), coll) )
		}
	this$xArity <-
		function () {
			x_( xArity(thunk_()) )
		}
	this
})

#' @param x any arbitrary value.

#' @export

x_ <- function (x) {
	# Collection any -> Arrow any
	# type constructor.

	if ('arrow' %in% class(x)) {
		x
	} else {
		structure(list(x = x), class = 'arrow')
	}
}

'$.arrow' <- function (obj, method) {
	# Arrow a -> symbol -> function
	# return an arrow method associated with the type a.

	method_name <- paste0(method)

	proto_ref <- 
		if (is.function( obj[['x']] )) {
			x_fn_proto
		} else if (is.vector( obj[['x']] )){
			x_coll_proto
		} else if (is.pairlist( obj[['x']] )) {
			x_coll_proto
		} else {
			object()
		}

	if ( !(method_name %in% ls(proto_ref)) ) {
		stop("the method does not exist")
	}

	fn <- proto_ref[[method_name]]

	environment(fn)[['thunk_']] <-
		function () obj[['x']]

	fn
}
