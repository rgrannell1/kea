
# -------------------------------- Collection methods -------------------------------- #

x_coll_proto <- local({

	this <- object()
	this$x <- 
		function () {
			reciever_()
		} 
	this$xAll <- 
		function (fn) {
			x_( xAll(fn, reciever_()) )
		}
	this$xAny <- 
		function (fn) {
			x_( xAny(fn, reciever_()) )
		}
	this$xApply <-
		function (fn) {
			x_( xApply(fn, reciever_()) )
		}
	this$xChars <-
		function () {
			x_( xChars(reciever_()) )
		}
	this$xConcatMap <-
		function (fn) {
			x_( xConcatMap(fn, reciever_()) )
		}
	this$xCollapse <-
		function (delim) {
			x_( xCollapse(delim, reciever_()) )
		}
	this$xCompress <-
		function () {
			x_( xCompress(reciever_()) )
		}
	this$xCount <-
		function (fn) {
			x_( xCount(fn, reciever_()) )
		}
	this$xDrop <-
		function (num) {
			x_( xDrop(num, reciever_()) )
		}
	this$xDropWhile <-
		function (fn) {
			x_( xDropWhile(fn, reciever_()) )
		}
	this$xFirst <-
		function () {
			x_( xFirst(reciever_()) )
		}
	this$xFoldl <-
		function (fn, init) {
			x_( xFoldl(fn, init, reciever_()) )
		}
	this$xFold <- this$xFoldl
	this$xFoldr <-
		function (fn, init) {
			x_( xFoldr(fn, init, reciever_()) )
		}
	this$xFourth <-
		function () {
			x_( xFourth(reciever_()) )
		}
	this$xInit <-
		function () {
			x_( xInit(reciever_()) )
		}
	this$xIsEmpty <-
		function () {
			x_( xIsEmpty(reciever_()) )
		}
	this$xIsFalse <-
		function () {
			x_( xIsFalse(reciever_()) )
		}
	this$xIsTrue <-
		function () {
			x_( xIsTrue(reciever_()) )
		}
	this$xIsNa <-
		function () {
			x_( xIsNa(reciever_()) )
		}
	this$xLast <-
		function () {
			x_( xLast(reciever_()) )
		}
	this$xLines <-
		function () {
			x_( xLines(reciever_()) )
		}
	this$xMap <-
		function (fn) {
			x_( xMap(fn, reciever_()) )
		}
	this$xMapAlong <-
		function (fn) {
			x_( xMapAlong(fn, reciever_()) )
		}
	this$xMapWhen <-
		function (pred, fn) {
			x_( xMapWhen(pred, fn, reciever_()) )
		}
	this$xNegate <-
		function () {
			x_( xNegate(reciever_()) )
		}
	this$xNotFalse <-
		function () {
			x_( xNotFalse(reciever_()) )
		}
	this$xNotTrue <-
		function () {
			x_( xNotTrue(reciever_()) )
		}
	this$xNotNa <-
		function () {
			x_( xNotNa(reciever_()) )
		}
	this$xPartition <-
		function (fn) {
			x_( xPartition(fn, reciever_()) )
		}
	this$xPositionl <-
		function (fn) {
			x_( xPositionl(fn, reciever_()) )
		}
	this$xPositionr <-
		function (fn) {
			x_( xPositionr(fn, reciever_()) )
		}
	this$xPred <-
		function () {
			x_( xPred(reciever_()) )
		}
	this$xReducel <-
		function (fn) {
			x_( xReducel(fn, reciever_()) )
		}
	this$xReduce <- 
		this$xReducel
	this$xReducer <-
		function (fn) {
			x_( xReducer(fn, reciever_()) )
		}
	this$xReject <-
		function (fn) {
			x_( xReject(fn, reciever_()) )
		}
	this$xRest <-
		function () {
			x_( xRest(reciever_()) )
		}
	this$xScanl <-
		function (fn, init) {
			x_( xScanl(fn, init, reciever_()) )
		}
	this$xSecond <-
		function () {
			x_( xSecond(reciever_()) )
		}
	this$xSegment <- 
		function (num) {
			x_( xSegment(num, reciever_()) )
		}
	this$xSelect <-
		function (fn) {
			x_( xSelect(fn, reciever_()) )
		}
	this$xSignum <-
		function () {
			x_( xSignum(reciever_()) )
		}
	this$xSplitAt <-
		function (ith) {
			x_( xSplitAt(ith, reciever_()) )
		}
	this$xSplitString <-
		function (regexp) {
			x_( xSplitString(regexp, reciever_()) )
		}
	this$xSplitWith <-
		function (pred) {
			x_( xSplitWith(pred, reciever_()) )
		}
	this$xSubString <-
		function (str) {
			x_( xSubString(str, reciever_()) )
		}
	this$xSucc <-
		function () {
			x_( xSucc(reciever_()) )
		}
	this$xTake <- 
		function (num) {
			x_( xTake(num, reciever_()) )
		}
	this$xTakeWhile <-
		function (pred) {
			x_( xTakeWhile(pred, reciever_()) )
		}
	this$xThird <-
		function () {
			x_( xThird(reciever_()) )
		}
	this$xUnchars <-
		function () {
			x_( xUnchars(reciever_()) )
		}
	this$xUnit <-
		function () {
			x_( xUnit(reciever_())	)	
		}
	this$xUnlines <- 
		function () {
			x_( xUnlines(reciever_()) )
		}
	this$xUntil <-
		function (pred, fn) {
			x_( xUntil(pred, fn, reciever_()) )
		}
	this$xUnwords <- 
		function () {
			x_( xUnwords(reciever_()) )
		}
	this$xUnzip <-
		function () {
			x_( xUnzip(reciever_()) )
		}
	this$xUnzipWith <-
		function (fn) {
			x_( xUnzipWith(fn, reciever_()) )
		}
	this$xWords <-
		function () {
			x_( xWords(reciever_()) )
		}
	this$xWhile <-
		function (pred, fn) {
			x_( xWhile(pred, fn, reciever_()) )
		}
	this$xZipWith <-
		function (fn, ...) {
			x_( xZipWith(fn, reciever_(), ...) )
		}
	this$xZip <- 
		function (...) {
			x_( xZip(reciever_(), ...) )
		}
	this
})

# -------------------------------- Function methods -------------------------------- #

x_fn_proto <- local({

	this <- object()
	this$x <- 
		function () {
			reciever_()
		} 
	this$xAsClosure <-
		function () {
			x_( xAsClosure(reciever_()) )
		}
	this$xAsUnary <-
		function () {
			x_( xAsUnary(reciever_()) )
		}
	this$xAsVariadic <-
		function () {
			x_( xAsVariadic(reciever_()) )
		}
	this$xAll <- 
		function (coll) {
			x_( xAll(reciever_(), coll) )
		}
	this$xAny <- 
		function (coll) {
			x_( xAny(reciever_(), coll) )
		}
	this$xApply <-
		function (coll) {
			x_( xApply(reciever_(), coll) )
		}
	this$xArity <-
		function () {
			x_( xArity(reciever_()) )
		}
	this$xConcatMap <-
		function (coll) {
			x_( xConcatMap(reciever_(), coll) )
		}
	this$xCount <-
		function (coll) {
			x_( xCount(reciever_(), coll) )
		}
	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(reciever_(), coll) )
		}
	this$xMap <-
		function (coll) {
			x_( xMap(reciever_(), coll) )
		}
	this$xMapAlong <-
		function (coll) {
			x_( xMapAlong(reciever_(), coll) )
		}
	this$xMapWhen <-
		function (fn, coll) {
			x_( xMapWhen(reciever_(), fn, coll) )
		}
	this$xFoldl <-
		function (init, coll) {
			x_( xFoldl(reciever_(), init, coll) )
		}
	this$xFoldr <-
		function (init, coll) {
			x_( xFoldr(reciever_(), init, coll) )
		}
	this$xFormals <-
		function () {
			x_( xFormals(reciever_()) )
		}
	this$xHasDefaults <-
		function () {
			x_( xHasDefaults(reciever_()) )
		}
	this$xIsVariadic <-
		function () {
			x_( xIsVariadic(reciever_()) )
		}
	this$xUntil <-
		function (fn, coll) {
			x_( xUntil(reciever_(), fn, coll) )
		}
	this$xUnzipWith <-
		function (colls) {
			x_( xUnzipWith(reciever_(), colls) )
		}
	this$xWhile <-
		function (fn, coll) {
			x_( xWhile(reciever_(), fn, coll) )
		}
	this$xZipWith <-
		function (...) {
			x_( xZipWith(reciever_(), ...) )
		}
	this$xZip <- 
		function (...) {
			x_( xZip(reciever_(), ...) )
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
	pcall <- paste0('$', method_name)

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

		say$method_not_found(
			pcall,
			list(
				x = obj[['x']],
				method = method_name))
	} else {
		fn <- proto_ref[[method_name]]

		environment(fn)[['reciever_']] <- 
			function () obj[['x']]
		fn		
	}
}
