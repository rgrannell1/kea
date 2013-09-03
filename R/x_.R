
# -------------------------------- Collection methods -------------------------------- #
#
# these methods can be called off of x_() objects, with one argument fixed.
# the meaning of the reciever function is changed upon the 
# invocation of an x_()$method, changing to the value of the x objects internal data.
# No object copying or anything else nasty is done.

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
	this$xAssoc <-
		function () {
			x_( xAssoc(reciever_()) )
		}
	# -------- B ------- #
	# -------- C ------- #
	this$xChars <-
		function () {
			x_( xChars(reciever_()) )
		}
	this$xCollapse <-
		function (delim) {
			x_( xCollapse(delim, reciever_()) )
		}
	this$xConst <-
		function (x) {
			x_( xConst(reciever_()) )
		}

	# -------- D ------- #
	this$xDissoc <-
		function () {
			x_( xAssoc(reciever_()) )
		}
	this$xDrop <-
		function (num) {
			x_( xDrop(num, reciever_()) )
		}
	this$xDo <-
		function (fn) {
			x_( xDo(fn, reciever_()) )
		}
	this$xDropWhile <-
		function (fn) {
			x_( xDropWhile(fn, reciever_()) )
		}
	# -------- E ------- #
	this$xExists <-
		function (pred, ...) {
			x_( xExists(pred, reciever_(), ...) )
		}
	# -------- F ------- #
	this$xFirst <-
		function () {
			x_( xFirst(reciever_()) )
		}
	this$xFoldl <-
		function (fn, init) {
			x_( xFoldl(fn, init, reciever_()) )
		}
	this$xFlatMap <-
		function (fn) {
			x_( xFlatMap(fn, reciever_()) )
		}
	this$xFlatten <-
		function (num) {
			x_( xFlatten(fn, reciever_()) )
		}
	this$xForall <-
		function (pred, ...) {
			xForall(pred, reciever_(), ...)
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
	this$xInsertBy <-
		function (fn, val) {
			x_( xInsertBy(fn, val, reciever_()) )
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

	this$xLocatel <-
		function (fn) {
			x_( xLocatel(fn, reciever_()) )
		}
	this$xLocate <-
		this$xLocatel
	this$xLocater <-
		function (fn) {
			x_( xLocater(fn, reciever_()) )
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
	this$xMapMany <-
		function (fn, ...) {
			x_( xMapMany(fn, reciever_(), ...) )
		}
	this$xMapWhen <-
		function (pred, fn) {
			x_( xMapWhen(pred, fn, reciever_()) )
		}
	# -------- N ------- #
	this$xName <-
		function (strs) {
			x_( xName(strs, reciever_()) ) 
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
	# -------- O ------- #
	# -------- P ------- #
	this$xPack <-
		function () {
			x_( xPack(reciever_()) )
		}
	this$xPoll <-
		function (fn) {
			x_( xPoll(fn, reciever_()) )
		}
	this$xPartial <- 
		function (fn) {
			x_( xPartial(fn, reciever_()) )
	}
	this$xPartition <-
		function (fn) {
			x_( xPartition(fn, reciever_()) )
		}
	this$xPred <-
		function () {
			x_( xPred(reciever_()) )
		}
	# -------- Q ------- #
	# -------- R ------- #
	this$xRecurMap <-
		function (fn) {
			x_( xRecurMap(fn, reciever_()) )
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
	# -------- S ------- #
	this$xScanl <-
		function (fn, init) {
			x_( xScanl(fn, init, reciever_()) )
		}
	this$xSecond <-
		function () {
			x_( xSecond(reciever_()) )
		}
	this$xSetProd <-
		function (...) {
			x_( xSetProd(reciever_(), ...) )
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
	this$xSplit <-
		function (ith) {
			x_( xSplit(ith, reciever_()) )
		}
	this$xShuffle <-
		function () {
			x_( xShuffle(reciever_()) )
		}
	this$xSplitStr <-
		function (rexp) {
			x_( xSplitStr(rexp, reciever_()) )
		}
	this$xSplitWith <-
		function (pred) {
			x_( xSplitWith(pred, reciever_()) )
		}
	this$xS. <- 
		this$xConst
	this$xSubStr <-
		function (str) {
			x_( xSubStr(str, reciever_()) )
		}
	this$xSucc <-
		function () {
			x_( xSucc(reciever_()) )
		}
	# -------- T ------- #
	this$xTap <-
		function (fn) {
			x_( fn(reciever_()) )
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
	this$xConst <-
		function (x) {
			x_( xConst(reciever_()) )
		}
	this$xCompose <- 
		function (fn1) {
			x_( xCompose(fn1, reciever_()) )
		}
	# -------- D ------- #

	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(reciever_(), coll) )
		}
	this$xDo <-
		function (coll) {
			x_( xDo(reciever_(), coll) )
		}
	# -------- E ------- #
	this$xExists <-
		function (...) {
			x_( xExists(reciever_(), ...) )
		}
	# -------- F ------- #
	this$xFixDefs <-
		function () {
			x_( xFixDefs(reciever_()) )
		}
	this$xFlip <- 
		function () {
			x_( xFlip(reciever_()) )
		}
	this$xFlatMap <-
		function (coll) {
			x_( xFlatMap(reciever_(), coll) )
		}
	this$xFoldl <-
		function (init, coll) {
			x_( xFoldl(reciever_(), init, coll) )
		}
	this$xForall <-
		function (...) {
			xForall(reciever_(), ...)
		}
	this$xFold <- this$xFoldl
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
	this$xHasDefs <-
		function () {
			x_( xHasDefs(reciever_()) )
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
	this$xInsertBy <-
		function (val, coll) {
			x_( xInsertBy(reciever_(), val, coll) )
		}
	# -------- J ------- #
	this$xJuxtapose <-
		function (...) {
			x_( xJuxtapose(reciever_(), ...) )
		}
	# -------- K ------- #
	this$xKestrel <-
		this$xConst
	this$xK <-
		this$xKestrel
	# -------- L ------- #
	this$xLimit <-
		function (num) {
			x_( xLimit(num, reciever_()) )
		}
	this$xLocatel <-
		function (coll) {
			x_( xLocatel(reciever_(), coll) )
		}
	this$xLocate <-
		this$xLocatel
	this$xLocater <-
		function (coll) {
			x_( xLocater(reciever_(), coll) )
		}
	# -------- M ------- #
	this$xMap <-
		function (coll) {
			x_( xMap(reciever_(), coll) )
		}
	this$xMapAlong <-
		function (coll) {
			x_( xMapAlong(reciever_(), coll) )
		}
	this$xMapMany <-
		function (...) {
			x_( xMapMany(reciever_(), ...) )
		}
	this$xMapWhen <-
		function (fn, coll) {
			x_( xMapWhen(reciever_(), fn, coll) )
		}
	this$xMinus <-
		function (fn2) {
			x_( xMinus(reciever_(), fn2) )
		}
	this$xMod <-
		function (fn2) {
			x_( xMod(reciever_(), fn2) )
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
	this$xPartition <-
		function (coll) {
			x_( xPartition(reciever_(), coll) )
		}
	this$xParams <-
		function () {
			x_( xParams(reciever_()) )
		}
	this$xPartMap <-
		function () {
			x_( xPartMap(reciever_()) )
		}
	this$xPartial <- 
		function (coll) {
			x_( xPartial(reciever_(), coll) )
		}
	this$xPoll <-
		function (coll) {
			x_( xPoll(reciever_(), coll) )
		}
	this$xPlus <-
		function (fn2) {
			x_( xPlus(reciever_(), fn2) )
		}
	this$xPhoenix <-
		this$xBiCompose
	this$xPhi <- 
		this$xBiCompose
	# -------- Q ------- #
	# -------- R ------- #
	this$xRecurMap <-
		function (coll) {
			x_( xRecurMap(reciever_(), coll) )
		}
	this$xReducel <-
		function (coll) {
			x_( xReducel(reciever_(), coll) )
		}
	this$xReduce <- 
		this$xReducel
	this$xReducer <-
		function (coll) {
			x_( xReducer(reciever_(), coll) )
		}
	this$xReject <-
		function (coll) {
			x_( xReject(reciever_(), coll) ) 
		}
	# -------- S ------- #
	this$xSelect <-
		function (coll) {
			x_( xSelect(reciever_(), coll) ) 
		}	
	this$xScanl <-
		function (init, coll) {
			x_( xScanl(reciever_(), init, coll) )
		}
	this$xSplitWith <-
		function (coll) {
			x_( xSplitWith(reciever_(), coll) )
		}
	this$xS. <-
		this$xBiCompose
	# -------- T ------- #
	this$xTap <-
		function (fn) {
			x_( fn(reciever_()) )
		}
	this$xTakeWhile <-
		function (coll) {
			x_( xTakeWhile(reciever_(), coll) )
		}
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
	this$xZipWith <-
		function (...) {
			x_( xZipWith(reciever_(), ...) )
		}
	this$xZip <- 
		function (...) {
			x_( xZip(reciever_(), ...) )
		}
	# -------- W ------- #
	this$xWait <-
		function (num) {
			x_( xWait(reciever_(), num) )
		}
	this$xWhile <-
		function (fn, coll) {
			x_( xWhile(reciever_(), fn, coll) )
		}
	this
})


#' @param val any arbitrary value.

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
		stop('method not found')
	} else {
		fn <- proto_ref[[method_name]]

		environment(fn)[['reciever_']] <- 
			function () obj[['x']]
		fn
	}
}
