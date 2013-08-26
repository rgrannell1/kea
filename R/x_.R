
# -------------------------------- Collection methods -------------------------------- #
# these methods can be called off of x_() objects, with one argument fixed.

x_coll_proto <- local({

	this <- object()
	this$x <- 
		function () {
			reciever_()
		} 
	# -------- A ------- #
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
	# -------- B ------- #
	# -------- C ------- #
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
	this$xConst <-
		function (val) {
			x_( xConst(reciever_()) )
		}
	this$xCount <-
		function (fn) {
			x_( xCount(fn, reciever_()) )
		}

	# -------- D ------- #
	this$xDrop <-
		function (num) {
			x_( xDrop(num, reciever_()) )
		}
	this$xDropWhile <-
		function (fn) {
			x_( xDropWhile(fn, reciever_()) )
		}
	# -------- E ------- #
	# -------- F ------- #
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
	# -------- G ------- #
	# -------- H ------- #
	# -------- I ------- #
	this$xIdentity <-
		function () {
			x_( xIdentity(reciever_()) )
		}
	this$xIdiotBird <-
		this$xIdentity
	this$xI <- 
		this$xIdiotBird
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
	# -------- J ------- #
	# -------- K ------- #
	this$xKestrel <-
		this$xConst
	# -------- L ------- #
	this$xLast <-
		function () {
			x_( xLast(reciever_()) )
		}
	this$xLines <-
		function () {
			x_( xLines(reciever_()) )
		}
	# -------- M ------- #
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
	# -------- N ------- #
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
	# -------- O ------- #
	# -------- P ------- #
	this$xPartial <- 
		function (fn) {
			x_( xPartial(fn, reciever_()) )
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
	# -------- Q ------- #
	# -------- R ------- #
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
	# -------- S ------- #
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
	this$xSprime <- 
		this$xConst
	this$xSubString <-
		function (str) {
			x_( xSubString(str, reciever_()) )
		}
	this$xSucc <-
		function () {
			x_( xSucc(reciever_()) )
		}
	# -------- T ------- #
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
	# -------- U ------- #
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
	# -------- V ------- #
	# -------- W ------- #
	this$xWords <-
		function () {
			x_( xWords(reciever_()) )
		}
	this$xWhile <-
		function (pred, fn) {
			x_( xWhile(pred, fn, reciever_()) )
		}
	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
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
	# -------- A ------- #
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
	this$xAnd <-
		function (pred2) {
			x_( xAnd(reciever_(), pred2) )
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
	# -------- B ------- #
	this$xBiCompose <-
		function (fn2, fn3) {
			x_( xBiCompose(reciever_(), fn2, fn3) )
		}
	this$xBy <-
		function (fn2) {
			x_( xBy(reciever_(), fn2) )
		}
	# -------- C ------- #
	this$xConcatMap <-
		function (coll) {
			x_( xConcatMap(reciever_(), coll) )
		}
	this$xCount <-
		function (coll) {
			x_( xCount(reciever_(), coll) )
		}
	this$xConst <-
		function (val) {
			x_( xConst(reciever_()) )
		}
	# -------- D ------- #
	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(reciever_(), coll) )
		}
	# -------- E ------- #
	# -------- F ------- #
	this$xFixDefaults <-
		function () {
			x_( xFixDefaults(reciever_()) )
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
	# -------- G ------- #
	# -------- H ------- #
	this$xHasDefaults <-
		function () {
			x_( xHasDefaults(reciever_()) )
		}
	# -------- I ------- #
	this$xIdentity <-
		function () {
			x_( xIdentity(reciever_()) )
		}
	this$xIdiotBird <-
		this$xIdentity
	this$xI <- 
		this$xIdiotBird
	this$xIsVariadic <-
		function () {
			x_( xIsVariadic(reciever_()) )
		}
	# -------- J ------- #
	# -------- K ------- #
	this$xKestrel <-
		this$xConst
	# -------- L ------- #
	# -------- M ------- #
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
	this$xMinus <-
		function (fn2) {
			x_( xMinus(reciever_(), fn2) )
		}
	# -------- N ------- #
	this$xNot <-
		function () {
			x_( xNot(reciever_()) )
		}
	# -------- O ------- #
	this$xOr <-
		function (pred2) {
			x_( xOr(reciever_(), pred2) )
		}
	this$xOver <-
		function (fn2) {
			x_( xOver(reciever_(), fn2) )
		}
	# -------- P ------- #
	this$xParameters <-
		function () {
			x_( xParameters(reciever_()) )
		}
	this$xParametres <-
		function () {
			x_( xParameters(reciever_()) )
		}
	this$xPartial <- 
		function (coll) {
			x_( xPartial(reciever_(), coll) )
		}
	this$xPlus <-
		function (fn2) {
			x_( xPlus(reciever_(), fn2) )
		}
	this$xPhi <- 
		this$xBiCompose
	# -------- Q ------- #
	# -------- R ------- #
	# -------- S ------- #
	this$xSprime <-
		this$xKestrel
	# -------- T ------- #
	# -------- U ------- #
	this$xUntil <-
		function (fn, coll) {
			x_( xUntil(reciever_(), fn, coll) )
		}
	this$xUnzipWith <-
		function (colls) {
			x_( xUnzipWith(reciever_(), colls) )
		}
	# -------- V ------- #
	# -------- W ------- #
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

x_ <- function (val) {
	# Collection any -> Arrow any
	# type constructor.

	if ('arrow' %in% class(val)) {
		val
	} else {
		structure(list(x = val), class = 'arrow')
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
