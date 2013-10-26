
# -------------------------------- Universal methods -------------------------------- #
#
# these prototypes contain methods that can be called by an x_() object, using an
# overloaded definition of the $ function.

# upon invocation of an x_()$method, the self_ function is updated to return the
# value contained in the x_() object. The self_ function should be unbound unless it is called by an x_() function,
# so an error is thrown if these prototypes are called directly.









x_any_proto <- local({

	this <- object()

	# -------- A ------- #

	# -------- B ------- #

	# -------- C ------- #

	# -------- D ------- #

	# -------- E ------- #
	this$xExecute <-
		function (fn) {
			# execute a side-effectful function
			# before using the previous x_ monad
			# for further chaining.

			fn()
			x_(self_())
		}
	# -------- F ------- #

	# -------- G ------- #
	this$xGraft <-
		function (str, fn) {
			# add a function to the x_
			# call chain for the
			# current R session.

			chainable <- function (...) {
				x_(fn(self_(), ...))
			}

			proto_ref <- get_proto_ref(self_())
			assign(str, chainable, envir = proto_ref)
		}
	# -------- H ------- #

	# -------- I ------- #
	this$xIdentity <-
		function () {
			x_( xIdentity(self_()) )
		}
	this$xIdiotBird <-
		this$xIdentity
	this$xI <-
		this$xIdiotBird
	# -------- J ------- #

	# -------- K ------- #

	# -------- L ------- #

	# -------- M ------- #

	# -------- N ------- #

	# -------- O ------- #

	# -------- P ------- #

	# -------- Q ------- #

	# -------- R ------- #

	# -------- S ------- #

	# -------- T ------- #
	this$xTap <-
		function (fn) {
			# call an arbitrary function with self,
			# effectively allowing anonymous function
			# to execute arbitrary code before shunting
			# the output data back into the x_ monad.

			x_( fn(self_()) )
		}
	# -------- U ------- #

	# -------- V ------- #
	this$xVersion <-
		function () {
			x_( xVersion() )
		}
	# -------- W ------- #

	# -------- X ------- #
	this$x <-
		function (mode = 'any') {
			as.vector(self_(), mode)
		}
	# -------- Y ------- #

	# -------- Z ------- #

	this
})










# -------------------------------- Not-quite-a-collection methods -------------------------------- #
#
# methods for non-canonical data types in arrow; data frames, tables, matrices and other odd and
# sometimes awkward structures. Almost exclusively conversion and reshaping methods.
#
#

x_matrix_proto <- local({

	this <- object()

	# -------- A ------- #
	this$xByCols <-
		function () {

			dims <- dim(self_())

			if (dims[1] == 0 && dims[0] == 0) {
				list()
			} else if (dims[2] == 0) {
				list()
			} else if (dims[1] == 0) {
				replicate( max(dims), list() )
			} else {
				x_( apply(self_(), 2, as.list) )
			}
		}

	this$xByRows <-
		function () {
			dims <- dim(self_())

			if (dims[1] == 0 && dims[0] == 0) {
				list()
			} else if (dims[1] == 0) {
				list()
			} else if (dims[2] == 0) {
				replicate( max(dims), list() )
			} else {
				x_( apply(self_(), 1, as.list) )
			}
		}
	# -------- B ------- #

	# -------- C ------- #
	this$xColUnit <-
		function () {
			x_(matrix(nrow = nrow(self_()), ncol = 0))
		}
	# -------- D ------- #

	# -------- E ------- #
	this$xElemsByCols <-
		function () {
			if (prod(dim(self_()) == 0)) {
				list()
			} else {
				x_( as.list(self_()) )
			}
		}
	this$xElemsByRows <-
		function () {
			if (prod(dim(self_()) == 0)) {
				list()
			} else {
				x_(as.list( t(self_()) ))
			}
		}
	# -------- F ------- #

	# -------- G ------- #

	# -------- H ------- #

	# -------- I ------- #

	# -------- J ------- #

	# -------- K ------- #

	# -------- L ------- #

	# -------- M ------- #

	# -------- N ------- #

	# -------- O ------- #

	# -------- P ------- #

	# -------- Q ------- #

	# -------- R ------- #
	this$xRowUnit <-
		function () {
			x_(matrix( nrow = 0, ncol = ncol(self_()) ))
		}
	# -------- S ------- #

	# -------- T ------- #
	this$xTranspose <-
		function () {
			x_( t(self_()) )
		}
	# -------- U ------- #
	this$xUnit <-
		function () {
			x_( matrix(nrow = 0, ncol = 0) )
		}
	# -------- V ------- #

	# -------- W ------- #

	# -------- X ------- #

	# -------- Y ------- #

	# -------- Z ------- #

	as.environment(
		c(as.list(this), as.list(x_any_proto)) )

})






x_data_frame_proto <- local({

	this <- object()

	# -------- A ------- #

	# -------- B ------- #
	this$xByCols <-
		function () {
			unname( as.list(self_()) )
		}
	# -------- C ------- #
	this$xColUnit <-
		function () {

		}
	# -------- D ------- #

	# -------- E ------- #

	# -------- F ------- #

	# -------- G ------- #

	# -------- H ------- #

	# -------- I ------- #

	# -------- J ------- #

	# -------- K ------- #

	# -------- L ------- #

	# -------- M ------- #

	# -------- N ------- #

	# -------- O ------- #

	# -------- P ------- #

	# -------- Q ------- #

	# -------- R ------- #
	this$xRowUnit <-
		function () {

		}
	# -------- S ------- #

	# -------- T ------- #

	# -------- U ------- #
	this$xUnit <-
		function () {

		}
	# -------- V ------- #

	# -------- W ------- #

	# -------- X ------- #

	# -------- Y ------- #

	# -------- Z ------- #

	as.environment(
		c(as.list(this), as.list(x_any_proto)) )

})







# -------------------------------- Collection methods -------------------------------- #
#
# methods specific to vectors, lists or pairlists.
#
#






x_coll_proto <- local({

	this <- object()
	# -------- A ------- #
	this$xAsFunction <-
		function () {
			x_( xAsFunction(self_()) )
		}
	this$xApply <-
		function (fn) {
			x_( xApply(fn, self_()) )
		}
	this$xAssoc <-
		function () {
			x_( xAssoc(self_()) )
		}
	# -------- B ------- #
	# -------- C ------- #
	this$xChars <-
		function () {
			x_( xChars(self_()) )
		}
	this$xCollapse <-
		function (str, ...) {
			x_( xCollapse(str, self_(), ...) )
		}
	this$xContains <-
		function (val) {
			x_( xContains(self_(), val) )
		}
	this$xConcat <-
		function (...) {
			x_( xConcat(self_(), ...) )
		}
	this$xCombinations <-
		function (num) {
			x_( xCombinations(num, self_()) )
		}
	this$xConst <-
		function () {
			x_( xConst(self_()) )
		}
	# -------- D ------- #
	this$xDissoc <-
		function () {
			x_( xAssoc(self_()) )
		}
	this$xDiffer <-
		function (coll2) {
			x_( xDiffer(self_(), coll2) )
		}
	this$xDrop <-
		function (num) {
			x_( xDrop(num, self_()) )
		}
	this$xDo <-
		function (fn) {
			x_( xDo(fn, self_()) )
		}
	this$xDropWhile <-
		function (pred) {
			x_( xDropWhile(pred, self_()) )
		}
	# -------- E ------- #
	this$xExists <-
		function (pred, ...) {
			x_( xExists(pred, self_(), ...) )
		}
	# -------- F ------- #
	this$xFirst <-
		function () {
			x_( xFirst(self_()) )
		}
	this$xFilter <-
		function (coll) {
			x_( xFilter(self_(), coll) )
		}
	this$xFoldl <-
		function (fn, init) {
			x_( xFoldl(fn, init, self_()) )
		}
	this$xFlatMap <-
		function (fn) {
			x_( xFlatMap(fn, self_()) )
		}
	this$xFlatten <-
		function (num) {
			x_( xFlatten(num, self_()) )
		}
	this$xForall <-
		function (pred, ...) {
			xForall(pred, self_(), ...)
		}
	this$xFold <- this$xFoldl
	this$xFoldr <-
		function (fn, init) {
			x_( xFoldr(fn, init, self_()) )
		}
	this$xFourth <-
		function () {
			x_( xFourth(self_()) )
		}
	# -------- G ------- #
	# -------- H ------- #
	# -------- I ------- #
	this$xInit <-
		function () {
			x_( xInit(self_()) )
		}
	this$xIsEmpty <-
		function () {
			x_( xIsEmpty(self_()) )
		}
	this$xIsFalse <-
		function () {
			x_( xIsFalse(self_()) )
		}
	this$xIsTrue <-
		function () {
			x_( xIsTrue(self_()) )
		}
	this$xIsNan <-
		function () {
			x_( xIsNan(self_()) )
		}
	this$xIsNa <-
		function () {
			x_( xIsNa(self_()) )
		}
	this$xIsNull <-
		function () {
			x_( xIsNull(self_()) )
		}
	# -------- J ------- #
	# -------- K ------- #
	this$xKestrel <-
		this$xConst
	# -------- L ------- #
	this$xLast <-
		function () {
			x_( xLast(self_()) )
		}
	this$xLength <-
		function () {
			x_( xLength(self_()) )
		}
	this$xLines <-
		function () {
			x_( xLines(self_()) )
		}

	this$xLocatel <-
		function (pred) {
			x_( xLocatel(pred, self_()) )
		}
	this$xLocate <-
		this$xLocatel
	this$xLocater <-
		function (pred) {
			x_( xLocater(pred, self_()) )
		}
	# -------- M ------- #
	this$xMap <-
		function (fn) {
			x_( xMap(fn, self_()) )
		}
	this$xMapAlong <-
		function (fn) {
			x_( xMapAlong(fn, self_()) )
		}
	this$xMapMany <-
		function (fn, ...) {
			x_( xMapMany(fn, self_(), ...) )
		}
	this$xMapWhen <-
		function (pred, fn) {
			x_( xMapWhen(pred, fn, self_()) )
		}
	# -------- N ------- #
	this$xName <-
		function (strs) {
			x_( xName(strs, coll = self_()) )
		}
	this$xNegate <-
		function () {
			x_( xNegate(self_()) )
		}
	this$xNotFalse <-
		function () {
			x_( xNotFalse(self_()) )
		}
	this$xNotTrue <-
		function () {
			x_( xNotTrue(self_()) )
		}
	this$xNotNa <-
		function () {
			x_( xNotNa(self_()) )
		}
	this$xNotNan <-
		function () {
			x_( xNotNan(self_()) )
		}
	# -------- O ------- #
	# -------- P ------- #
	this$xPack <-
		function () {
			x_( xPack(self_()) )
		}
	this$xPoll <-
		function (pred) {
			x_( xPoll(pred, self_()) )
		}
	this$xPartial <-
		function (fn) {
			x_( xPartial(fn, self_()) )
	}
	this$xPluck <-
		function (str) {
			x_( xPluck(str, self_()) )
		}
	this$xPartition <-
		function (pred) {
			x_( xPartition(pred, self_()) )
		}
	this$xPartitionWith <-
		function (pred) {
			x_( xPartitionWith(pred, self_()) )
		}
	this$xPred <-
		function () {
			x_( xPred(self_()) )
		}
	# -------- Q ------- #
	# -------- R ------- #
	this$xRecurMap <-
		function (fn) {
			x_( xRecurMap(fn, self_()) )
		}
	this$xReducel <-
		function (fn) {
			x_( xReducel(fn, self_()) )
		}
	this$xReduce <-
		this$xReducel
	this$xReducer <-
		function (fn) {
			x_( xReducer(fn, self_()) )
		}
	this$xRepeat <-
		function (num) {
			x_( xRepeat(num, self_()) )
		}
	this$xReject <-
		function (pred) {
			x_( xReject(pred, self_()) )
		}
	this$xRest <-
		function () {
			x_( xRest(self_()) )
		}
	# -------- S ------- #
	this$xScanl <-
		function (fn, init) {
			x_( xScanl(fn, init, self_()) )
		}
	this$xSecond <-
		function () {
			x_( xSecond(self_()) )
		}
	this$xSetProd <-
		function (...) {
			x_( xSetProd(self_(), ...) )
		}
	this$xSegment <-
		function (num) {
			x_( xSegment(num, self_()) )
		}
	this$xSelect <-
		function (pred) {
			x_( xSelect(pred, self_()) )
		}
	this$xSignum <-
		function () {
			x_( xSignum(self_()) )
		}
	this$xSplit <-
		function (num) {
			x_( xSplit(num, self_()) )
		}
	this$xShuffle <-
		function () {
			x_( xShuffle(self_()) )
		}
	this$xSplitStr <-
		function (rexp) {
			x_( xSplitStr(rexp, self_()) )
		}
	this$xSplitWith <-
		function (pred) {
			x_( xSplitWith(pred, self_()) )
		}
	this$xS. <-
		this$xConst
	this$xSubStr <-
		function (str) {
			x_( xSubStr(str, self_()) )
		}
	this$xSucc <-
		function () {
			x_( xSucc(self_()) )
		}
	# -------- T ------- #
	this$xTake <-
		function (num) {
			x_( xTake(num, self_()) )
		}
	this$xTakeWhile <-
		function (pred) {
			x_( xTakeWhile(pred, self_()) )
		}
	this$xThird <-
		function () {
			x_( xThird(self_()) )
		}
	this$xThread <-
		function (...) {
			x_( xThread(self_(), ...) )
		}
	# -------- U ------- #
	this$xUnchars <-
		function () {
			x_( xUnchars(self_()) )
		}
	this$xUnion <-
		function (coll2) {
			x_( xUnion(self_(), coll2) )
		}
	this$xUnit <-
		function () {
			x_( xUnit(self_())	)
		}
	this$xUnlines <-
		function () {
			x_( xUnlines(self_()) )
		}
	this$xUntil <-
		function (pred, fn) {
			x_( xUntil(pred, fn, self_()) )
		}
	this$xUnwords <-
		function () {
			x_( xUnwords(self_()) )
		}
	this$xUnfold <-
		function (fn, pred) {
			x_( xUnfold(fn, pred, self_()) )
		}
	this$xUnfoldl <-
		this$xUnfold
	# -------- V ------- #
	# -------- W ------- #
	this$xWords <-
		function () {
			x_( xWords(self_()) )
		}
	this$xWhile <-
		function (pred, fn) {
			x_( xWhile(pred, fn, self_()) )
		}
	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
	this$xZipWith <-
		function (fn, ...) {
			x_( xZipWith(fn, self_(), ...) )
		}
	this$xZip <-
		function (...) {
			x_( xZip(self_(), ...) )
		}

	as.environment(
		c(as.list(this), as.list(x_any_proto)) )
})









# -------------------------------- Function methods -------------------------------- #
#
# These methods operate on functions wrapped in the arrow object.
# I anticipate that these methods will be less used, but there's no reason to exclude them
# entirely. Methods such as Apply work nicely with this style.







x_fn_proto <- local({

	this <- object()
	this$x <-
		function () {
			self_()
		}

	# -------- A ------- #
	this$xAsClosure <-
		function () {
			x_( xAsClosure(self_()) )
		}
	this$xAsUnary <-
		function () {
			x_( xAsUnary(self_()) )
		}
	this$xAsVariadic <-
		function () {
			x_( xAsVariadic(self_()) )
		}
	this$xAndLift <-
		function (pred2) {
			x_( xAndLift(self_(), pred2) )
		}
	this$xApply <-
		function (coll) {
			x_( xApply(self_(), coll) )
		}
	this$xArity <-
		function () {
			x_( xArity(self_()) )
		}
	# -------- B ------- #
	this$xCardinal <-
		function () {
			x_( xFlip(self_()) )
		}
	this$xBiCompose <-
		function (fn2, fn3) {
			x_( xBiCompose(self_(), fn2, fn3) )
		}
	this$xByLift <-
		function (fn2) {
			x_( xByLift(self_(), fn2) )
		}
	# -------- C ------- #
	this$xC <-
		this$xCardinal
	this$xConst <-
		function () {
			x_( xConst(self_()) )
		}
	this$xCompose <-
		function (fn1) {
			x_( xCompose(fn1, self_()) )
		}
	# -------- D ------- #
	this$xDifferLift <-
		function (fn2) {
			x_( xDifferLift(fn1, fn2) )
		}
	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(self_(), coll) )
		}
	this$xDo <-
		function (coll) {
			x_( xDo(self_(), coll) )
		}
	# -------- E ------- #
	this$xEqualLift <-
		function (fn2) {
			x_( xEqualLift(self_(), fn2) )
		}
	this$xExists <-
		function (...) {
			x_( xExists(self_(), ...) )
		}
	# -------- F ------- #
	this$xFixDefs <-
		function () {
			x_( xFixDefs(self_()) )
		}
	this$xFilter <-
		function (pred) {
			x_( xFilter(pred, self_()) )
		}
	this$xFlip <-
		this$xCardinal
	this$xFlatMap <-
		function (coll) {
			x_( xFlatMap(self_(), coll) )
		}
	this$xFoldl <-
		function (init, coll) {
			x_( xFoldl(self_(), init, coll) )
		}
	this$xForall <-
		function (...) {
			xForall(self_(), ...)
		}
	this$xFold <- this$xFoldl
	this$xFoldr <-
		function (init, coll) {
			x_( xFoldr(self_(), init, coll) )
		}
	this$xFormals <-
		function () {
			x_( xFormals(self_()) )
		}
	# -------- G ------- #

	# -------- H ------- #
	this$xHasDefs <-
		function () {
			x_( xHasDefs(self_()) )
		}
	# -------- I ------- #
	this$xInterLift <-
		function (fn2) {
			x_( xInterLift(self_(), fn2) )
		}
	this$xIsVariadic <-
		function () {
			x_( xIsVariadic(self_()) )
		}
	# -------- J ------- #
	this$xJuxtapose <-
		function (...) {
			x_( xJuxtapose(self_(), ...) )
		}
	# -------- K ------- #
	this$xKestrel <-
		this$xConst
	this$xK <-
		this$xKestrel
	# -------- L ------- #
	this$xLimit <-
		function (num) {
			x_( xLimit(num, self_()) )
		}
	this$xLocatel <-
		function (coll) {
			x_( xLocatel(self_(), coll) )
		}
	this$xLocate <-
		this$xLocatel
	this$xLocater <-
		function (coll) {
			x_( xLocater(self_(), coll) )
		}
	# -------- M ------- #
	this$xMap <-
		function (coll) {
			x_( xMap(self_(), coll) )
		}
	this$xMapAlong <-
		function (coll) {
			x_( xMapAlong(self_(), coll) )
		}
	this$xMapMany <-
		function (...) {
			x_( xMapMany(self_(), ...) )
		}
	this$xMapWhen <-
		function (fn, coll) {
			x_( xMapWhen(self_(), fn, coll) )
		}
	this$xMinusLift <-
		function (fn2) {
			x_( xMinusLift(self_(), fn2) )
		}
	this$xModLift <-
		function (fn2) {
			x_( xModLift(self_(), fn2) )
		}
	# -------- N ------- #
	this$xNot <-
		function () {
			x_( xNot(self_()) )
		}
	# -------- O ------- #
	this$xOrLift <-
		function (pred2) {
			x_( xOrLift(self_(), pred2) )
		}
	this$xOverLift <-
		function (fn2) {
			x_( xOverLift(self_(), fn2) )
		}
	# -------- P ------- #
	this$xPartition <-
		function (coll) {
			x_( xPartition(self_(), coll) )
		}
	this$xPartitionWith <-
		function (coll) {
			x_( xPartitionWith(self_(), coll) )
		}
	this$xParams <-
		function () {
			x_( xParams(self_()) )
		}
	this$xPartMap <-
		function () {
			x_( xPartMap(self_()) )
		}
	this$xPartial <-
		function (coll) {
			x_( xPartial(self_(), coll) )
		}
	this$xPoll <-
		function (coll) {
			x_( xPoll(self_(), coll) )
		}
	this$xPlusLift <-
		function (fn2) {
			x_( xPlusLift(self_(), fn2) )
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
			x_( xRecurMap(self_(), coll) )
		}
	this$xReducel <-
		function (coll) {
			x_( xReducel(self_(), coll) )
		}
	this$xReduce <-
		this$xReducel
	this$xReducer <-
		function (coll) {
			x_( xReducer(self_(), coll) )
		}
	this$xReject <-
		function (coll) {
			x_( xReject(self_(), coll) )
		}
	# -------- S ------- #
	this$xS. <-
		this$xBiCompose
	this$xSelect <-
		function (coll) {
			x_( xSelect(self_(), coll) )
		}
	this$xScanl <-
		function (init, coll) {
			x_( xScanl(self_(), init, coll) )
		}
	this$xSplitWith <-
		function (coll) {
			x_( xSplitWith(self_(), coll) )
		}

	# -------- T ------- #
	this$xTap <-
		function (fn) {
			x_( fn(self_()) )
		}
	this$xTakeWhile <-
		function (coll) {
			x_( xTakeWhile(self_(), coll) )
		}

	this$xThrush <-
		function (fn2) {
			x_( xWrap(self_(), fn2) )
		}
	this$xT <-
		this$xThrush
	# -------- U ------- #
	this$xUntil <-
		function (fn, init) {
			x_( xUntil(self_(), fn, init) )
		}
	this$xUnionLift <-
		function (fn2) {
			x_( xUnionLift(self_(), fn2) )
		}
	this$xUnfold <-
		function (fn, init) {
			x_( xUnfold(self_(), fn, init) )
		}
		this$xUnfoldl <-
			this$xUnfold
	# -------- V ------- #
	this$xZipWith <-
		function (...) {
			x_( xZipWith(self_(), ...) )
		}
	this$xZip <-
		function (...) {
			x_( xZip(self_(), ...) )
		}
	# -------- W ------- #
	this$xWait <-
		function (num) {
			x_( xWait(self_(), num) )
		}
	this$xWhile <-
		function (fn, init) {
			x_( xWhile(self_(), fn, init) )
		}
	this$xWrap <-
		this$xThrush


	as.environment(
		c(as.list(this), as.list(x_any_proto)) )
})









get_proto_ref <- function (val) {
	# get the reference to the appropriate
	# methods.

	proto_ref <-
	if (is.function( val )) {
		x_fn_proto
	} else if (is.vector( val ) || is.pairlist( val )){
		x_coll_proto
	} else if (is.matrix( val )) {
		x_matrix_proto
	} else {
		x_any_proto
	}
}

suggest_similar_method <- function (val, method_name, pcall) {

	proto_ref <- get_proto_ref(val)

	similar <-
		agrep(
			pattern = method_name,
			x = ls(proto_ref),
			fixed = False,
			value = True,
			max.distance = list(
				cost = 0.07
			),
			cost = list(
				deletions = 4,
				insertions = 2,
				substitutions = 1
			))

	# a cheap and nasty heuristic for finding the 'best' match.

	similar <- similar[ which.min(nchar(similar)) ]

	assert(
		method_name %in% ls(proto_ref), pcall,
		exclaim$method_not_found(method_name, similar))
}

# -------------------------------- Type Constructor -------------------------------- #

#' x_
#'
#' Generate a chainable arrow object, that can use methods.
#'
#' @param val a function, collection, or arbitrary value.
#'
#' @return an object of class "arrow", with a single field 'x' that contains val.
#'
#' @section Corner Cases:
#' 		The methods that can be used by x_() object varies depending on the type of val.
#' 		Some methods are specific to functions or collections. If a non-function and non-collection is
#' 		supplied then very few methods can be used.
#'
#' 		Because the definition of $ was overloaded to allow method chaining, the
#' 		field 'x' inside an arrow object cannot be accessed using x_()$x. Writing
#'		x_()$x() is required.
#'
#' @details
#'
#' Creating arrow objects is efficient, since no methods are copied on instantiation. Invoking an arrow
#' method (using $) has a small amount overhead, since the definition of $
#' has been overloading to allow method calling.
#'
#' @template glossary
#'
#' @example inst/examples/blank.R
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

	proto_ref <- get_proto_ref( obj[['x']] )

	if (!method_name %in% ls(proto_ref)) {
		suggest_similar_method(
			obj[['x']], method_name, pcall)
	}

	fn <- proto_ref[[method_name]]
	environment(fn)[['self_']] <- function () obj[['x']]
	fn
}
