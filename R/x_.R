
# -------------------------------- Collection methods -------------------------------- #

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
	this$xCollapse <-
		function (delim) {
			x_( xCollapse(delim, thunk_()) )
		}
	this$xCompress <-
		function () {
			x_( xCompress(thunk_()) )
		}
	this$xCount <-
		function (fn) {
			x_( xCount(fn, thunk_()) )
		}
	this$xDrop <-
		function (num) {
			x_( xDrop(num, thunk_()) )
		}
	this$xDropWhile <-
		function (fn) {
			x_( xDropWhile(fn, thunk_()) )
		}
	this$xFirst <-
		function () {
			x_( xFirst(thunk_()) )
		}
	this$xFoldl <-
		function (fn, init) {
			x_( xFoldl(fn, init, thunk_()) )
		}
	this$xFoldr <-
		function (fn, init) {
			x_( xFoldr(fn, init, thunk_()) )
		}
	this$xFourth <-
		function () {
			x_( xFourth(thunk_()) )
		}
	this$xInit <-
		function () {
			x_( xInit(thunk_()) )
		}
	this$xIsEmpty <-
		function () {
			x_( xIsEmpty(thunk_()) )
		}
	this$xIsFalse <-
		function () {
			x_( xIsFalse(thunk_()) )
		}
	this$xIsTrue <-
		function () {
			x_( xIsTrue(thunk_()) )
		}
	this$xIsNa <-
		function () {
			x_( xIsNa(thunk_()) )
		}
	this$xLast <-
		function () {
			x_( xLast(thunk_()) )
		}
	this$xLines <-
		function () {
			x_( xLines(thunk_()) )
		}
	this$xNegate <-
		function () {
			x_( xNegate(thunk_()) )
		}
	this$xNotFalse <-
		function () {
			x_( xNotFalse(thunk_()) )
		}
	this$xNotTrue <-
		function () {
			x_( xNotTrue(thunk_()) )
		}
	this$xNotNa <-
		function () {
			x_( xNotNa(thunk_()) )
		}
	this$xPartition <-
		function (fn) {
			x_( xPartition(fn, thunk_()) )
		}
	this$xPositionl <-
		function (fn) {
			x_( xPositionl(fn, thunk_()) )
		}
	this$xPositionr <-
		function (fn) {
			x_( xPositionr(fn, thunk_()) )
		}
	this$xPred <-
		function () {
			x_( xPred(thunk_()) )
		}
	this$xReducel <-
		function (fn) {
			x_( xReducel(fn, thunk_()) )
		}
	this$xReducer <-
		function (fn) {
			x_( xReducer(fn, thunk_()) )
		}
	this$xReject <-
		function (fn) {
			x_( xReject(fn, thunk_()) )
		}
	this$xRest <-
		function () {
			x_( xRest(thunk_()) )
		}
	this$xScanl <-
		function (fn, init) {
			x_( xScanl(fn, init, thunk_()) )
		}
	this$xSecond <-
		function () {
			x_( xSecond(thunk_()) )
		}
	this$xSelect <-
		function (fn) {
			x_( xSelect(fn, thunk_()) )
		}
	this$xSignum <-
		function () {
			x_( xSignum(thunk_()) )
		}
	this$xSplitAt <-
		function (ith) {
			x_( xSplitAt(ith, thunk_()) )
		}
	this$xSplitString <-
		function (regexp) {
			x_( xSplitString(regexp, thunk_()) )
		}
	this$xSplitWith <-
		function (pred) {
			x_( xSplitWith(pred, thunk_()) )
		}
	this$xSubString <-
		function (str) {
			x_( xSubString(str, thunk_()) )
		}
	this$xSucc <-
		function () {
			x_( xSucc(thunk_()) )
		}
	this$xTake <- 
		function (num) {
			x_( xTake(num, thunk_()) )
		}
	this$xTakeWhile <-
		function (pred) {
			x_( xTakeWhile(pred, thunk_()) )
		}
	this$xThird <-
		function () {
			x_( xThird(thunk_()) )
		}
	this$xUnchars <-
		function () {
			x_( xUnchars(thunk_()) )
		}
	this$xUnit <-
		function () {
			x_( xUnit(thunk_())	)	
		}
	this$xUnlines <- 
		function () {
			x_( xUnlines(thunk_()) )
		}
	this$xUnwords <- 
		function () {
			x_( xUnwords(thunk_()) )
		}
	this$xWords <-
		function () {
			x_( xWords(thunk_()) )
		}
	this
})

# -------------------------------- Function methods -------------------------------- #

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
	this$xCount <-
		function (coll) {
			x_( xCount(thunk_(), coll) )
		}
	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(thunk_(), coll) )
		}
	this$xFoldl <-
		function (init, coll) {
			x_( xFoldl(thunk_(), init, coll) )
		}
	this$xFoldr <-
		function (init, coll) {
			x_( xFoldr(thunk_(), init, coll) )
		}
	this$xFormals <-
		function () {
			x_( xFormals(thunk_()) )
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
		stop("the8")
	}

	fn <- proto_ref[[method_name]]

	environment(fn)[['thunk_']] <-
		function () obj[['x']]

	fn
}
