
# -------------------------------- Collection methods -------------------------------- #
#
# these methods can be called off of x_() objects, with one argument fixed.
# the meaning of the reciever function is changed upon the 
# invocation of an x_()$method, changing to the value of the x objects internal data.
# No object copying or anything else nasty is done.

x_coll_proto <- local({

	this <- object()
	this$x <- 
		function (mode = 'any') {
			as.vector(reciever_(), mode)
		} 
	# -------- A ------- #
	this$xAll <- 
		function (pred) {
			x_( xAll(pred, reciever_()) )
		}
	this$xAny <- 
		function (pred) {
			x_( xAny(pred, reciever_()) )
		}
	this$xAsFunction <- 
		function () {
			x_( xAsFunction(reciever_()) )
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
		function (str, ...) {
			x_( xCollapse(str, reciever_(), ...) )
		}
	this$xContains <- 
		function (val) {
			x_( xContains(reciever_(), val) )
		}
	this$xConcat <-
		function (...) {
			x_( xConcat(reciever_(), ...) )
		}
	this$xCombinations <-
		function (num) {
			x_( xCombinations(num, reciever_()) )
		}
	this$xConst <-
		function () {
			x_( xConst(reciever_()) )
		}

	# -------- D ------- #
	this$xDissoc <-
		function () {
			x_( xAssoc(reciever_()) )
		}
	this$xDiffer <-
		function (coll2) {
			x_( xDiffer(reciever_(), coll2) )
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
		function (pred) {
			x_( xDropWhile(pred, reciever_()) )
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
			x_( xFlatten(num, reciever_()) )
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
	this$xIsNull <-
		function () {
			x_( xIsNull(reciever_()) )
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
	this$xLength <-
		function () {
			x_( xLength(reciever_()) )
		}
	this$xLines <-
		function () {
			x_( xLines(reciever_()) )
		}

	this$xLocatel <-
		function (pred) {
			x_( xLocatel(pred, reciever_()) )
		}
	this$xLocate <-
		this$xLocatel
	this$xLocater <-
		function (pred) {
			x_( xLocater(pred, reciever_()) )
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
			x_( xName(strs, coll = reciever_()) )
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
		function (pred) {
			x_( xPoll(pred, reciever_()) )
		}
	this$xPartial <- 
		function (fn) {
			x_( xPartial(fn, reciever_()) )
	}
	this$xPluck <-
		function (str) {
			x_( xPluck(str, reciever_()) )
		}

	this$xPartition <-
		function (pred) {
			x_( xPartition(pred, reciever_()) )
		}		
	this$xPartitionWith <-
		function (pred) {
			x_( xPartitionWith(pred, reciever_()) )
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
	this$xRepeat <-
		function (num) {
			x_( xRepeat(num, reciever_()) )
		}
	this$xReject <-
		function (pred) {
			x_( xReject(pred, reciever_()) )
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
		function (pred) {
			x_( xSelect(pred, reciever_()) )
		}
	this$xSignum <-
		function () {
			x_( xSignum(reciever_()) )
		}
	this$xSplit <-
		function (num) {
			x_( xSplit(num, reciever_()) )
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
	this$xSwap <- 
		function (fn) {
			x_( xSwap(fn, reciever_()) )
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
	this$xThread <- 
		function (...) {
			x_( xThread(reciever_(), ...) )
		}
	# -------- U ------- #
	this$xUnchars <-
		function () {
			x_( xUnchars(reciever_()) )
		}
	this$xUnion <- 
		function (coll2) {
			x_( xUnion(reciever_(), coll2) )
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
	this$xUnfold <- 
		function (fn, pred) {
			x_( xUnfold(fn, pred, reciever_()) )
		}
	this$xUnfoldl <-
		this$xUnfold
	# -------- V ------- #
	this$xVersion <-
		function () {
			x_( xVersion() )
		}
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
	this$xAndLift <-
		function (pred2) {
			x_( xAndLift(reciever_(), pred2) )
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
	this$xCardinal <- 
		function () {
			x_( xFlip(reciever_()) )
		}
	this$xBiCompose <-
		function (fn2, fn3) {
			x_( xBiCompose(reciever_(), fn2, fn3) )
		}
	this$xByLift <-
		function (fn2) {
			x_( xByLift(reciever_(), fn2) )
		}
	# -------- C ------- #
	this$xC <-
		this$xCardinal
	this$xConst <-
		function () {
			x_( xConst(reciever_()) )
		}
	this$xCompose <- 
		function (fn1) {
			x_( xCompose(fn1, reciever_()) )
		}
	# -------- D ------- #
	this$xDifferLift <-
		function (fn2) {
			x_( xDifferLift(fn1, fn2) )
		}
	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(reciever_(), coll) )
		}
	this$xDo <-
		function (coll) {
			x_( xDo(reciever_(), coll) )
		}
	# -------- E ------- #
	this$xEqualLift <-
		function (fn2) {
			x_( xEqualLift(reciever_(), fn2) )
		}
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
		this$xCardinal
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
	this$xInterLift <-
		function (fn2) {
			x_( xInterLift(reciever_(), fn2) )
		}
	this$xIsVariadic <-
		function () {
			x_( xIsVariadic(reciever_()) )
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
	this$xMinusLift <-
		function (fn2) {
			x_( xMinusLift(reciever_(), fn2) )
		}
	this$xModLift <-
		function (fn2) {
			x_( xModLift(reciever_(), fn2) )
		}		
	# -------- N ------- #
	this$xNot <-
		function () {
			x_( xNot(reciever_()) )
		}
	# -------- O ------- #
	this$xOrLift <-
		function (pred2) {
			x_( xOrLift(reciever_(), pred2) )
		}
	this$xOverLift <-
		function (fn2) {
			x_( xOverLift(reciever_(), fn2) )
		}
	# -------- P ------- #
	this$xPartition <-
		function (coll) {
			x_( xPartition(reciever_(), coll) )
		}
	this$xPartitionWith <-
		function (coll) {
			x_( xPartitionWith(reciever_(), coll) )
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
	this$xPlusLift <-
		function (fn2) {
			x_( xPlusLift(reciever_(), fn2) )
		}
	this$xPhoenix <-
		this$xBiCompose
	this$xPhi <- 
		this$xBiCompose
	# -------- Q ------- #
	this$xQueer <-
		this$xCompose
	this$xQ <-
		this$xCompose
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
	this$xS. <-
		this$xBiCompose
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

	# -------- T ------- #
	this$xTap <-
		function (fn) {
			x_( fn(reciever_()) )
		}
	this$xTakeWhile <-
		function (coll) {
			x_( xTakeWhile(reciever_(), coll) )
		}

	this$xThrush <-
		function (fn2) {
			x_( xWrap(reciever_(), fn2) )
		}
	this$xT <-
		this$xThrush
	# -------- U ------- #
	this$xUntil <-
		function (fn, init) {
			x_( xUntil(reciever_(), fn, init) )
		}
	this$xUnionLift <-
		function (fn2) {
			x_( xUnionLift(reciever_(), fn2) )
		}
	this$xUnfold <-
		function (fn, init) {
			x_( xUnfold(reciever_(), fn, init) )
		}
		this$xUnfoldl <-
			this$xUnfold
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
		function (fn, init) {
			x_( xWhile(reciever_(), fn, init) )
		}
	this$xWrap <-
		this$xThrush
	this
})







# -------------------------------- Type Constructor -------------------------------- #




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

	assert(
		method_name %in% ls(proto_ref), pcall)

	fn <- proto_ref[[method_name]]

	environment(fn)[['reciever_']] <- function () obj[['x']]
	fn
}
