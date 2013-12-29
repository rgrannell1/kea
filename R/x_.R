
# -------------------------------- x_( ) -------------------------------- #
#
# The x_() function is a constructor that wraps a datum, and allows methods
# to be called on that datum. The constructor returns a monad, so the
# methods can be chained indefinetly until the $x() method is used to pull the
# data out of the monad (much like unlist for the list monad).
#
# The methods that are attatched to a datum depends on its class;
# these blocks of methods are stored in "proto" objects, that
# consist of the methods, a private object, and methods inherited from
# a universal method object.
#
# upon invocation of an x_()$method, the self_ function is updated to return the
# value contained in the x_() object. The self_ function should be unbound unless it is called by an x_() function,
# so an error is thrown if these prototypes are called directly. This workaround is to keep memory usage
# low by only customising the method when it is being called, rather than when the arrow monad is created.

# Inheritance Diagram
#
# any proto-- ---------------- x_[ any ]
#            |
#            other proto ----- x_[ other ]
#

# there are currently three flavours of method:
# chaining methods: xMethod. These return an x_ monad to chain off.
# chaining variadic methods: xMethod... . There are similar, but take variadic arguments.
# non-chaining methods: x_Methods. These exit the monad.
# non-chaining methods: x_Methods... . These are variadic, and exit the monad into the land of normal types.



# -------------------------------- Universal methods -------------------------------- #
#
# these prototypes contain methods that can be called by an x_() object, using an
# overloaded definition of the $ function.

x_any_proto <- local({

	this <- Object()
	this$private <- list(
		contents_are = "arbitrary values"
	)

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

	this$x_Execute <-
		function (fn) {
			# execute a side-effectful function
			# before using the previous x_ monad
			# for further chaining.

			fn()
			self_()
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
	this$xI <-
		this$xIdentity

	this$x_Identity <-
		function () {
			xIdentity(self_())
		}
	this$x_I <-
		this$x_Identity

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
	this$x_Version <-
		function () {
			xVersion()
		}
	# -------- W ------- #

	# -------- X ------- #
	this$x <-
		function () {
			self_()
		}
	# -------- Y ------- #

	# -------- Z ------- #

	this
})










# -------------------------------- Not-quite-a-collection methods -------------------------------- #
#
# methods for non-canonical data types in arrow; data frames, tables, matrices and other odd and
# sometimes awkward structures. I don't want these to be treated as first-class citizens (particularily data frames),
# but there is no reason they shouldn't have methods to convert them into a list representation.
#

x_matrix_proto <- local({

	this <- Object()

	# -------- A ------- #
	# -------- B ------- #
	this$xByCols <-
		function () {
			dims <- dim(self_())

			if (dims[1] == 0 && dims[0] == 0) {
				x_( list() )
			} else if (dims[2] == 0) {
				x_( list() )
			} else if (dims[1] == 0) {
				x_( replicate(max(dims), list()))
			} else {
				x_( apply(self_(), 2, as.list) )
			}
		}

	this$x_ByCols <-
		function () {
			dims <- dim(self_())

			if (dims[1] == 0 && dims[0] == 0) {
				list()
			} else if (dims[2] == 0) {
				list()
			} else if (dims[1] == 0) {
				replicate(max(dims), list())
			} else {
				apply(self_(), 2, as.list)
			}
		}

	this$xByRows <-
		function () {
			dims <- dim(self_())

			if (dims[1] == 0 && dims[0] == 0) {
				x_( list() )
			} else if (dims[1] == 0) {
				x_( list() )
			} else if (dims[2] == 0) {
				x_( replicate(max(dims), list()) )
			} else {
				x_( apply(self_(), 1, as.list) )
			}
		}

	this$x_ByRows <-
		function () {
			dims <- dim(self_())

			if (dims[1] == 0 && dims[0] == 0) {
				list()
			} else if (dims[1] == 0) {
				list()
			} else if (dims[2] == 0) {
				replicate(max(dims), list())
			} else {
				apply(self_(), 1, as.list)
			}
		}

	# -------- C ------- #
	this$xColUnit <-
		function () {
			x_(matrix(nrow = nrow(self_()), ncol = 0))
		}
	this$x_ColUnit <-
		function () {
			matrix(nrow = nrow(self_()), ncol = 0)
		}
	# -------- D ------- #

	# -------- E ------- #
	this$xElemsByCols <-
		function () {
			if (prod(dim(self_()) == 0)) {
				x_( list() )
			} else {
				x_( as.xLinest(self_()) )
			}
		}
	this$x_ElemsByCols <-
		function () {
			if (prod(dim(self_()) == 0)) {
				x_( list() )
			} else {
				x_( as.xLinest(self_()) )
			}
		}


	this$xElemsByRows <-
		function () {
			if (prod(dim(self_()) == 0)) {
				x_( list() )
			} else {
				x_(as.list( t(self_()) ))
			}
		}
	this$x_ElemsByRows <-
		function () {
			if (prod(dim(self_()) == 0)) {
				list()
			} else {
				as.list( t(self_()) )
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
	this$x_RowUnit <-
		function () {
			matrix( nrow = 0, ncol = ncol(self_()) )
		}
	# -------- S ------- #

	# -------- T ------- #
	this$xTranspose <-
		function () {
			x_( t(self_()) )
		}
	this$x_Transpose <-
		function () {
			t(self_())
		}
	# -------- U ------- #
	this$xUnit <-
		function () {
			x_( matrix(nrow = 0, ncol = 0) )
		}
	this$x_Unit <-
		function () {
			matrix(nrow = 0, ncol = 0)
		}
	# -------- V ------- #

	# -------- W ------- #

	# -------- X ------- #

	# -------- Y ------- #

	# -------- Z ------- #

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "matrices"
	)
	this
})






x_data_frame_proto <- local({

	this <- Object()
	this$private <- list(
		contents_are = "data.frames"
	)

	# -------- A ------- #

	# -------- B ------- #
	this$xByCols <-
		function () {
			x_(unname( as.list(self_()) ))
		}
	this$x_ByCols <-
		function () {
			unname( as.list(self_()) )
		}
	# -------- C ------- #
	this$xColUnit <-
		function () {

		}
	this$x_ColUnit <-
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
	this$x_RowUnit <-
		function () {

		}

	# -------- S ------- #

	# -------- T ------- #

	# -------- U ------- #
	this$xUnit <-
		function () {

		}
	this$x_Unit <-
		function () {

		}

	# -------- V ------- #

	# -------- W ------- #

	# -------- X ------- #

	# -------- Y ------- #

	# -------- Z ------- #

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "data.frames"
	)
	this
})







# -------------------------------- Collection methods -------------------------------- #
#
# methods specific to vectors, lists or pairlists.
#
#






x_coll_proto <- local({

	this <- Object()

	# -------- A ------- #

	# --- xAsFunction --- #
	this$xAsFunction <-
		function () {
			x_( xAsFunction(self_()) )
		}
	this$xAsFunction... <-
		function (...) {
			x_(  xAsFunction...(self_(), ...) )
		}

	this$x_AsFunction <-
		function () {
			xAsFunction(self_())
		}
	this$x_AsFunction... <-
		function (...) {
			 xAsFunction...(self_(), ...)
		}

	# --- xApply --- #
	this$xApply <-
		function (fn) {
			x_( xApply(fn, self_()) )
		}
	this$xApply... <-
		function (fn, ...) {
			x_( xApply...(fn, self_(), ...) )
		}

	this$x_Apply <-
		function (fn) {
			xApply(fn, self_())
		}
	this$x_Apply... <-
		function (fn, ...) {
			xApply...(fn, self_(), ...)
		}

	# --- xAssoc --- #
	this$xAssoc <-
		function () {
			x_( xAssoc(self_()) )
		}
	this$xAssoc... <-
		function (...) {
			x_( xAssoc...(self_(), ...) )
		}

	this$x_Assoc <-
		function () {
			xAssoc(self_())
		}
	this$x_Assoc... <-
		function (...) {
			xAssoc...(self_(), ...)
		}

	# -------- B ------- #
	# -------- C ------- #

	# --- xChars --- #
	this$xChars <-
		function () {
			x_( xChars(self_()) )
		}
	this$x_Chars <-
		function () {
			xChars(self_())
		}

	# --- xCombos --- #
	this$xCombos <-
		function (num) {
			x_( xCombos(self_(), num) )
		}
	this$xCombos... <-
		function (num, ...) {
			x_( xCombos...(num, self_(), ...) )
		}

	this$x_Combos <-
		function (num) {
			xCombos(self_(), num)
		}
	this$x_Combos... <-
		function (num, ...) {
			xCombos...(num, self_(), ...)
		}

	# --- xConst --- #
	this$xConst <-
		function () {
			x_( xConst(self_()) )
		}

	this$x_Const <-
		function () {
			xConst(self_())
		}

	# -------- D ------- #
	# --- xDissoc --- #
	this$xDissoc <-
		function () {
			x_( xAssoc(self_()) )
		}
	this$xDissoc <-
		function (...) {
			x_( xAssoc(self_(), ...) )
		}

	this$x_Dissoc <-
		function () {
			xAssoc(self_())
		}
	this$x_Dissoc <-
		function (...) {
			xAssoc(self_(), ...)
		}

	# --- xDiffer --- #
	this$xDiffer <-
		function () {
			x_( xDiffer(self_()) )
		}

	this$x_Differ <-
		function () {
			xDiffer(self_())
		}

	this$xDiffer... <-
		function (...) {
			x_( xDiffer...(self_(), ...) )
		}

	this$x_Differ... <-
		function (...) {
			xDiffer...(self_(), ...)
		}

	# --- xDrop --- #
	this$xDrop <-
		function (num) {
			x_( xDrop(num, self_()) )
		}
	this$xDrop... <-
		function (num, ...) {
			x_( xDrop...(num, self_(), ...) )
		}

	this$x_Drop <-
		function (num) {
			xDrop(num, self_())
		}
	this$x_Drop... <-
		function (num, ...) {
			xDrop...(num, self_(), ...)
		}

	# --- xDo --- #
	this$xDo <-
		function (fn) {
			x_( xDo(fn, self_()) )
		}
	this$xDo... <-
		function (fn, ...) {
			x_( xDo(fn, self_(), ...) )
		}

	this$x_Do <-
		function (fn) {
			Do(fn, self_())
		}
	this$x_Do... <-
		function (fn, ...) {
			Do(fn, self_(), ...)
		}

	# --- xDropWhile --- #
	this$xDropWhile <-
		function (pred) {
			x_( xDropWhile(pred, self_()) )
		}
	this$xDropWhile... <-
		function (pred, ...) {
			x_( xDropWhile...(pred, self_(), ...) )
		}

	this$x_DropWhile <-
		function (pred) {
			xDropWhile(pred, self_())
		}
	this$x_DropWhile... <-
		function (pred, ...) {
			xDropWhile...(pred, self_(), ...)
		}
	# -------- E ------- #

	# --- xExists --- #
	this$xExists <-
		function (pred) {
			x_( xExists(pred, self_()) )
		}
	this$xExists... <-
		function (pred, ...) {
			x_( xExists...(pred, self_(), ...) )
		}

	this$x_Exists <-
		function (pred) {
			xExists(pred, self_())
		}
	this$x_Exists... <-
		function (pred, ...) {
			xExists...(pred, self_(), ...)
		}

	# --- xExplode --- #
	this$xExplode <-
		function (rexp) {
			x_( xExplode(rexp, self_()) )
		}
	this$x_Explode <-
		function (rexp) {
			xExplode(rexp, self_())
		}

	# -------- F ------- #

	# --- xFirst --- #
	this$xFirst <-
		function () {
			x_( xFirst(self_()) )
		}
	this$xFirst... <-
		function (...) {
			x_( xFirst...(self_(), ...) )
		}

	this$x_First <-
		function () {
			xFirst(self_())
		}
	this$x_First... <-
		function (...) {
			xFirst...(self_(), ...)
		}

	# --- xFilter --- #
	this$xFilter <-
		function (pred) {
			x_( xFilter(pred, self_()) )
		}
	this$xFilter... <-
		function (pred, ...) {
			x_( xFilter...(pred, self_(), ...) )
		}

	this$x_Filter <-
		function (pred) {
			xFilter(pred, self_())
		}
	this$x_Filter... <-
		function (pred, ...) {
			xFilter...(pred, self_(), ...)
		}

	# --- xFoldl --- #
	this$xFoldl <-
		function (fn, init) {
			x_( xFoldl(fn, init, self_()) )
		}
	this$xFoldl... <-
		function (fn, init, ...) {
			x_( xFoldl...(fn, init, self_(), ...) )
		}

	this$x_Foldl <-
		function (fn, init) {
			xFoldl(fn, init, self_())
		}
	this$x_Foldl... <-
		function (fn, init, ...) {
			xFoldl...(fn, init, self_(), ...)
		}
	# --- xFold --- #
	this$xFold <-
		this$xFoldl
	this$xFold... <-
		this$xFoldl...

	this$x_Fold <-
		this$x_Foldl
	this$x_Fold... <-
		this$x_Foldl...

	# --- xFlatMap --- #
	this$xFlatMap <-
		function (fn) {
			x_( xFlatMap(fn, self_()) )
		}
	this$xFlatMap... <-
		function (fn, ...) {
			x_( xFlatMap...(fn, self_(), ...) )
		}

	this$x_FlatMap <-
		function (fn) {
			xFlatMap(fn, self_())
		}
	this$x_FlatMap... <-
		function (fn, ...) {
			xFlatMap...(fn, self_(), ...)
		}

	# --- xFlatten --- #
	this$xFlatten <-
		function (num) {
			x_( xFlatten(num, self_()) )
		}
	this$xFlatten... <-
		function (num, ...) {
			x_( xFlatten...(num, self_(), ...) )
		}

	this$x_Flatten <-
		function (num) {
			xFlatten(num, self_())
		}
	this$x_Flatten... <-
		function (num, ...) {
			xFlatten...(num, self_(), ...)
		}

	# --- xForall --- #
	this$xForall <-
		function (pred) {
			x_( xForall(pred, self_()) )
		}
	this$xForall... <-
		function (pred, ...) {
			x_( xForall...(pred, self_(), ...) )
		}

	this$x_Forall <-
		function (pred) {
			xForall(pred, self_())
		}
	this$x_Forall... <-
		function (pred, ...) {
			xForall...(pred, self_(), ...)
		}


	# --- xFoldr --- #
	this$xFoldr <-
		function (fn, init) {
			x_( xFoldr(fn, init, self_()) )
		}
	this$xFoldr... <-
		function (fn, init, ...) {
			x_( xFoldr...(fn, init, self_(), ...) )
		}

	this$x_Foldr <-
		function (fn, init) {
			xFoldr(fn, init, self_())
		}
	this$x_Foldr... <-
		function (fn, init, ...) {
			xFoldr...(fn, init, self_(), ...)
		}

	# --- xFoldListl --- #
	this$xFoldListl <-
		function (fn, init) {
			x_( xFoldListl(fn, init, self_()) )
		}
	this$xFoldListl... <-
		function (fn, init, ...) {
			x_( xFoldListl...(fn, init, self_(), ...) )
		}

	this$x_FoldListl <-
		function (fn, init) {
			xFoldListl(fn, init, self_())
		}
	this$x_FoldListl... <-
		function (fn, init, ...) {
			xFoldListl...(fn, init, self_(), ...)
		}

	this$xFoldList <-
		function (fn, init) {
			x_( xFoldList(fn, init, self_()) )
		}
	this$xFoldList... <-
		function (fn, init, ...) {
			x_( xFoldList...(fn, init, self_(), ...) )
		}

	this$x_FoldList <-
		function (fn, init) {
			xFoldList(fn, init, self_())
		}
	this$x_FoldList... <-
		function (fn, init, ...) {
			xFoldList...(fn, init, self_(), ...)
		}

	# --- xFourth --- #
	this$xFourth <-
		function () {
			x_( xFourth(self_()) )
		}
	this$xFourth... <-
		function (...) {
			x_( xFourth...(self_(), ...) )
		}

	this$x_Fourth <-
		function () {
			xFourth(self_())
		}
	this$x_Fourth... <-
		function (...) {
			xFourth...(self_(), ...)
		}
	# -------- G ------- #
	this$xGet <-
		function () {
			x_( xGet(self_()) )
		}
	this$x_Get <-
		function () {
			xGet(self_())
		}

	# -------- H ------- #
	# -------- I ------- #

	# --- xImplode --- #
	this$xImplode <-
		function (str) {
			x_( xImplode(str, self_() ))
		}
	this$xImplode... <-
		function (str, ...) {
			x_( xImplode...(str, self_(), ...) )
		}

	this$x_Implode <-
		function (str) {
			xImplode(str, self_() )
		}
	this$x_Implode... <-
		function (str, ...) {
			xImplode...(str, self_(), ...)
		}

	# --- xIsMember --- #
	this$xIsMember <-
		function (val) {
			x_( xIsMember(val, self_()) )
		}
	this$xIsMember... <-
		function (..., val) {
			x_( xIsMember...(val, self_(), ...) )
		}

	this$x_IsMember <-
		function (val) {
			xIsMember(val, self_())
		}
	this$x_IsMember... <-
		function (..., val) {
			xIsMember...(val, self_(), ...)
		}

	# --- xInit --- #
	this$xInit <-
		function () {
			x_( xInit(self_()) )
		}
	this$xInit... <-
		function (...) {
			x_( xInit...(self_(), ...) )
		}

	this$x_Init <-
		function () {
			xInit(self_())
		}
	this$x_Init... <-
		function (...) {
			xInit...(self_(), ...)
		}

	# --- xIsEmpty --- #
	this$xIsEmpty <-
		function () {
			x_( xIsEmpty(self_()) )
		}
	this$xIsEmpty... <-
		function (...) {
			x_( xIsEmpty...(self_(), ...) )
		}

	this$x_IsEmpty <-
		function () {
			xIsEmpty(self_())
		}
	this$x_IsEmpty... <-
		function (...) {
			xIsEmpty...(self_(), ...)
		}

	# --- xIsFalse --- #
	this$xIsFalse <-
		function () {
			x_( xIsFalse(self_()) )
		}
	this$xIsFalse... <-
		function (...) {
			x_( xIsFalse...(self_(), ...) )
		}

	this$x_IsFalse <-
		function () {
			xIsFalse(self_())
		}
	this$x_IsFalse... <-
		function (...) {
			xIsFalse...(self_(), ...)
		}

	# --- xIsTrue --- #
	this$xIsTrue <-
		function () {
			x_( xIsTrue(self_()) )
		}
	this$xIsTrue... <-
		function (...) {
			x_( xIsTrue...(self_(), ...) )
		}

	this$x_IsTrue <-
		function () {
			xIsTrue(self_())
		}
	this$x_IsTrue... <-
		function (...) {
			xIsTrue...(self_(), ...)
		}

	# --- xIsNan --- #
	this$xIsNan <-
		function () {
			x_( xIsNan(self_()) )
		}
	this$xIsNan... <-
		function (...) {
			x_( xIsNan...(self_(), ...) )
		}

	this$x_IsNan <-
		function () {
			xIsNan(self_())
		}
	this$x_IsNan... <-
		function (...) {
			xIsNan...(self_(), ...)
		}

	# --- xIsNa --- #
	this$xIsNa <-
		function () {
			x_( xIsNa(self_()) )
		}
	this$xIsNa... <-
		function (...) {
			x_( xIsNa...(self_(), ...) )
		}

	this$x_IsNa <-
		function () {
			xIsNa(self_())
		}
	this$x_IsNa... <-
		function (...) {
			xIsNa...(self_(), ...)
		}

	# --- xIsNull --- #
	this$xIsNull <-
		function () {
			x_( xIsNull(self_()) )
		}
	this$xIsNull... <-
		function (...) {
			x_( xIsNull...(self_(), ...) )
		}

	this$x_IsNull <-
		function () {
			xIsNull(self_())
		}
	this$x_IsNull... <-
		function (...) {
			xIsNull...(self_(), ...)
		}

	# --- xIterate --- #
	this$xIterate <-
		function (fn) {
			x_( xIterate(fn, self_()) )
		}
	this$x_Iterate <-
		function (fn) {
			xIterate(fn, self_())
		}

	# --- xInter --- #
	this$xInter <-
		function () {
			x_( xInter(self_()) )
		}

	this$x_Inter <-
		function () {
			xInter(self_())
		}

	this$xInter... <-
		function (...) {
			x_( xInter...(self_(), ...) )
		}

	this$x_Inter... <-
		function (...) {
			xInter...(self_(), ...)
		}

	# -------- J ------- #
	# --- xJoin --- #
	this$xJoin <-
		function () {
			x_( xJoin(self_()) )
		}
	this$xJoin... <-
		function (...) {
			x_( xJoin...(self_(), ...) )
		}

	this$x_Join <-
		function () {
			xJoin(self_())
		}
	this$x_Join... <-
		function (...) {
			xJoin...(self_(), ...)
		}

	# --- xJuxtapose --- #
	this$xJuxtapose <-
		function (fns) {
			x_( xJuxtapose(self_()) )
		}

	this$x_Juxtapose <-
		function (fns) {
			xJuxtapose(self_())
		}
	# -------- K ------- #
	# -------- L ------- #
	# --- xLast --- #
	this$xLast <-
		function () {
			x_( xLast(self_()) )
		}
	this$xLast... <-
		function (...) {
			x_( xLast...(self_(), ...) )
		}

	this$x_Last <-
		function () {
			xLast(self_())
		}
	this$x_Last... <-
		function (...) {
			xLast...(self_(), ...)
		}

	# --- xLength --- #
	this$xLength <-
		function () {
			x_( xLength(self_()) )
		}
	this$xLength... <-
		function (...) {
			x_( xLength...(self_(), ...) )
		}

	this$x_Length <-
		function () {
			xLength(self_())
		}
	this$x_Length... <-
		function (...) {
			xLength...(self_(), ...)
		}
	# --- xLimit --- #
	this$xLimit <-
		function (fn) {
			x_( xLimit(fn, self_()) )
		}
	this$x_Limit <-
		function (fn) {
			xLimit(fn, self_())
		}

	# --- xLines --- #
	this$xLines <-
		function () {
			x_( xLines(self_()) )
		}
	this$x_Lines <-
		function () {
			xLines(self_())
		}

	# --- xLocatel --- #
	this$xLocatel <-
		function (pred) {
			x_( xLocatel(pred, self_()) )
		}
	this$xLocatel... <-
		function (pred, ...) {
			x_( xLocatel...(pred, self_(), ...) )
		}

	this$x_Locatel <-
		function (pred) {
			xLocatel(pred, self_())
		}
	this$x_Locatel... <-
		function (pred, ...) {
			xLocatel...(pred, self_(), ...)
		}

	this$xLocate <-
		this$xLocatel
	this$xLocate... <-
		this$xLocatel...

	this$x_Locate <-
		this$x_Locatel
	this$x_Locate... <-
		this$x_Locatel...

	# --- xLocater --- #
	this$xLocater <-
		function (pred) {
			x_( xLocater(pred, self_()) )
		}
	this$xLocater... <-
		function (pred, ...) {
			x_( xLocater...(pred, self_(), ...) )
		}

	this$x_Locater <-
		function (pred) {
			xLocater(pred, self_())
		}
	this$x_Locater... <-
		function (pred, ...) {
			xLocater...(pred, self_(), ...)
		}

	# -------- M ------- #
	# --- xMap --- #
	this$xMap <-
		function (fn) {
			x_( xMap(fn, self_()) )
		}
	this$xMap... <-
		function (fn, ...) {
			x_( xMap...(fn, self_(), ...) )
		}

	this$x_Map <-
		function (fn) {
			xMap(fn, self_())
		}
	this$x_Map... <-
		function (fn, ...) {
			xMap...(fn, self_(), ...)
		}
	# --- xMapply --- #
	this$xMapply <-
		function (fn) {
			x_( xMapply(fn, self_()) )
		}
	this$xMapply... <-
		function (fn, ...) {
			x_( xMapply...(fn, self_(), ...) )
		}

	this$x_Mapply <-
		function (fn) {
			xMapply(fn, self_())
		}
	this$x_Mapply... <-
		function (fn, ...) {
			xMapply...(fn, self_(), ...)
		}

	# --- xMapIndexed --- #
	this$xMapIndexed <-
		function (fn) {
			x_( xMapIndexed(fn, self_()) )
		}
	this$xMapIndexed... <-
		function (fn, ...) {
			x_( xMapIndexed...(fn, self_(), ...) )
		}

	this$x_MapIndexed <-
		function (fn) {
			xMapIndexed(fn, self_())
		}
	this$x_MapIndexed... <-
		function (fn, ...) {
			xMapIndexed...(fn, self_(), ...)
		}

	# --- xMapMany --- #
	this$xMapMany <-
		function (fn) {
			x_( xMapMany(fn, self_()) )
		}
	this$xMapMany... <-
		function (fn, ...) {
			x_( xMapMany...(fn, self_(), ...) )
		}

	this$x_MapMany <-
		function (fn) {
			xMapMany(fn, self_())
		}
	this$x_MapMany... <-
		function (fn, ...) {
			xMapMany...(fn, self_(), ...)
		}

	# --- xMapWhen --- #
	this$xMapWhen <-
		function (pred, fn) {
			x_( xMapWhen(pred, fn, self_()) )
		}
	this$xMapWhen... <-
		function (pred, fn, ...) {
			x_( xMapWhen...(pred, fn, self_(), ...) )
		}

	this$x_MapWhen <-
		function (pred, fn) {
			xMapWhen(pred, fn, self_())
		}
	this$x_MapWhen... <-
		function (pred, fn, ...) {
			xMapWhen...(pred, fn, self_(), ...)
		}

	# -------- N ------- #
	# --- xName --- #
	this$xName <-
		function (strs) {
			x_( xName(strs, coll = self_()) )
		}

	this$x_Name <-
		function (strs) {
			xName(strs, coll = self_())
		}

	# --- xNegate --- #
	this$xNegate <-
		function () {
			x_( xNegate(self_()) )
		}
	this$xNegate... <-
		function (...) {
			x_( xNegate...(self_(), ...) )
		}

	this$x_Negate <-
		function () {
			xNegate(self_())
		}
	this$x_Negate... <-
		function (...) {
			xNegate...(self_(), ...)
		}

	# --- xNotFalse --- #
	this$xNotFalse <-
		function () {
			x_( xNotFalse(self_()) )
		}
	this$xNotFalse... <-
		function (...) {
			x_( xNotFalse...(self_(), ...) )
		}

	this$x_NotFalse <-
		function () {
			xNotFalse(self_())
		}
	this$x_NotFalse... <-
		function (...) {
			xNotFalse...(self_(), ...)
		}

	# --- xNotTrue --- #
	this$xNotTrue <-
		function () {
			x_( xNotTrue(self_()) )
		}
	this$xNotTrue... <-
		function (...) {
			x_( xNotTrue...(self_(), ...) )
		}

	this$x_NotTrue <-
		function () {
			xNotTrue(self_())
		}
	this$x_NotTrue... <-
		function (...) {
			xNotTrue...(self_(), ...)
		}

	# --- xNotNa --- #
	this$xNotNa <-
		function () {
			x_( xNotNa(self_()) )
		}
	this$xNotNa... <-
		function (...) {
			x_( xNotNa...(self_(), ...) )
		}

	this$x_NotNa <-
		function () {
			xNotNa(self_())
		}
	this$x_NotNa... <-
		function (...) {
			xNotNa...(self_(), ...)
		}

	# --- xNotNan --- #
	this$xNotNan <-
		function () {
			x_( xNotNan(self_()) )
		}
	this$xNotNan... <-
		function (...) {
			x_( xNotNan...(self_(), ...) )
		}

	this$x_NotNan <-
		function () {
			xNotNan(self_())
		}
	this$x_NotNan... <-
		function (...) {
			xNotNan...(self_(), ...)
		}

	# -------- O ------- #
	# -------- P ------- #
	# --- xPack --- #
	this$xPack <-
		function () {
			x_( xPack(self_()) )
		}
	this$xPack... <-
		function (...) {
			x_( xPack...(self_(), ...) )
		}

	this$x_Pack <-
		function () {
			xPack(self_())
		}
	this$x_Pack... <-
		function (...) {
			xPack...(self_(), ...)
		}

	# --- xPoll --- #
	this$xPoll <-
		function (pred) {
			x_( xPoll(pred, self_()) )
		}
	this$xPoll... <-
		function (pred, ...) {
			x_( xPoll...(pred, self_(), ...) )
		}

	this$x_Poll <-
		function (pred) {
			xPoll(pred, self_())
		}
	this$x_Poll... <-
		function (pred, ...) {
			xPoll...(pred, self_(), ...)
		}

	# --- xPartial --- #
	this$xPartial <-
		function (fn) {
			x_( xPartial(fn, self_()) )
	}
	this$xPartial... <-
		function (fn, ...) {
			x_( xPartial...(fn, self_(), ...) )
	}

	this$x_Partial <-
		function (fn) {
			xPartial(fn, self_())
	}
	this$x_Partial... <-
		function (fn, ...) {
			xPartial...(fn, self_(), ...)
	}

	# --- xPluck --- #
	this$xPluck <-
		function (str) {
			x_( xPluck(str, self_()) )
		}
	this$xPluck... <-
		function (str, ...) {
			x_( xPluck...(str, self_(), ...) )
		}

	this$x_Pluck <-
		function (str) {
			xPluck(str, self_())
		}
	this$x_Pluck... <-
		function (str, ...) {
			xPluck...(str, self_(), ...)
		}

	# --- xPartition --- #
	this$xPartition <-
		function (pred) {
			x_( xPartition(pred, self_()) )
		}
	this$x_Partition <-
		function (pred) {
			xPartition(pred, self_())
		}

	this$xPartition... <-
		function (pred, ...) {
			x_( xPartition...(pred, self_(), ...) )
		}

	this$x_Partition... <-
		function (pred, ...) {
			xPartition...(pred, self_(), ...)
		}

	# --- xPermute --- #
	this$xPermute <-
		function (coll) {
			x_( xPermute(coll, self_()) )
		}
	this$xPermute... <-
		function (coll, ...) {
			x_( xPermute(coll, self_(), ...) )
		}

	this$x_Permute <-
		function (coll) {
			xPermute(coll, self_())
		}
	this$x_Permute... <-
		function (coll, ...) {
			xPermute(coll, self_(), ...)
		}

	# --- xPred --- #
	this$xPred <-
		function () {
			x_( xPred(self_()) )
		}
	this$xPred... <-
		function (...) {
			x_( xPred...(self_(), ...) )
		}

	this$x_Pred <-
		function () {
			xPred(self_())
		}
	this$x_Pred... <-
		function (...) {
			xPred...(self_(), ...)
		}

	# -------- Q ------- #
	# -------- R ------- #
	# --- xRecurMap --- #
	this$xRecurMap <-
		function (fn) {
			x_( xRecurMap(fn, self_()) )
		}
	this$xRecurMap... <-
		function (fn, ...) {
			x_( xRecurMap...(fn, self_(), ...) )
		}

	this$x_RecurMap <-
		function (fn) {
			xRecurMap(fn, self_())
		}
	this$x_RecurMap... <-
		function (fn, ...) {
			xRecurMap...(fn, self_(), ...)
		}

	# --- xReducel --- #
	this$xReducel <-
		function (fn) {
			x_( xReducel(fn, self_()) )
		}
	this$xReducel... <-
		function (fn, ...) {
			x_( xReducel...(fn, self_(), ...) )
		}

	this$x_Reducel <-
		function (fn) {
			xReducel(fn, self_())
		}
	this$x_Reducel... <-
		function (fn, ...) {
			xReducel...(fn, self_(), ...)
		}

	this$xReduce <-
		this$xReducel
	this$xReduce... <-
		this$xReducel...

	this$x_Reduce <-
		this$x_Reducel
	this$x_Reduce... <-
		this$x_Reducel...

	# --- xReducer --- #
	this$xReducer <-
		function (fn) {
			x_( xReducer(fn, self_()) )
		}
	this$xReducer... <-
		function (fn, ...) {
			x_( xReducer...(fn, self_(), ...) )
		}

	this$x_Reducer <-
		function (fn) {
			xReducer(fn, self_())
		}
	this$x_Reducer... <-
		function (fn, ...) {
			xReducer...(fn, self_(), ...)
		}

	# --- xRepeat --- #
	this$xRepeat <-
		function (num) {
			x_( xRepeat(num, self_()) )
		}
	this$xRepeat... <-
		function (num, ...) {
			x_( xRepeat...(num, self_(), ...) )
		}

	this$x_Repeat <-
		function (num) {
			xRepeat(num, self_())
		}
	this$x_Repeat... <-
		function (num, ...) {
			xRepeat...(num, self_(), ...)
		}

	# --- xReject --- #
	this$xReject <-
		function (pred) {
			x_( xReject(pred, self_()) )
		}
	this$xReject... <-
		function (pred, ...) {
			x_( xReject...(pred, self_(), ...) )
		}

	this$x_Reject <-
		function (pred) {
			xReject(pred, self_())
		}
	this$x_Reject... <-
		function (pred, ...) {
			xReject...(pred, self_(), ...)
		}

	# --- xRest --- #
	this$xRest <-
		function () {
			x_( xRest(self_()) )
		}
	this$xRest... <-
		function (...) {
			x_( xRest...(self_(), ...) )
		}

	this$x_Rest <-
		function () {
			xRest(self_())
		}
	this$x_Rest... <-
		function (...) {
			xRest...(self_(), ...)
		}

	# --- xRest --- #
	this$xReverse <-
		function () {
			x_( xReverse(self_()) )
		}
	this$xReverse... <-
		function (...) {
			x_( xReverse...(self_(), ...) )
		}

	this$x_Reverse <-
		function () {
			xReverse(self_())
		}
	this$x_Reverse... <-
		function (...) {
			xReverse...(self_(), ...)
		}
	# -------- S ------- #
	# --- xSecond --- #
	this$xSecond <-
		function () {
			x_( xSecond(self_()) )
		}
	this$xSecond... <-
		function (...) {
			x_( xSecond...(self_(), ...) )
		}

	this$x_Second <-
		function () {
			xSecond(self_())
		}
	this$x_Second... <-
		function (...) {
			xSecond...(self_(), ...)
		}

	# --- xSetProd --- #
	this$xSetProd <-
		function () {
			x_( xSetProd(self_()) )
		}
	this$xSetProd... <-
		function (...) {
			x_( xSetProd...(self_(), ...) )
		}

	this$x_SetProd <-
		function () {
			xSetProd(self_())
		}
	this$x_SetProd... <-
		function (...) {
			xSetProd...(self_(), ...)
		}

	# --- xSegment --- #
	this$xSegment <-
		function (num) {
			x_( xSegment(num, self_()) )
		}
	this$xSegment... <-
		function (num, ...) {
			x_( xSegment...(num, self_(), ...) )
		}

	this$x_Segment <-
		function (num) {
			xSegment(num, self_())
		}
	this$x_Segment... <-
		function (num, ...) {
			xSegment...(num, self_(), ...)
		}

	# --- xSelect --- #
	this$xSelect <-
		function (pred) {
			x_( xSelect(pred, self_()) )
		}
	this$xSelect... <-
		function (pred, ...) {
			x_( xSelect...(pred, self_(), ...) )
		}

	this$x_Select <-
		function (pred) {
			xSelect(pred, self_())
		}
	this$x_Select... <-
		function (pred, ...) {
			xSelect...(pred, self_(), ...)
		}

	# --- xSignum --- #
	this$xSignum <-
		function () {
			x_( xSignum(self_()) )
		}
	this$xSignum... <-
		function (...) {
			x_( xSignum...(self_(), ...) )
		}

	this$x_Signum <-
		function () {
			xSignum(self_())
		}
	this$x_Signum... <-
		function (...) {
			xSignum...(self_(), ...)
		}

	# --- xSplit --- #
	this$xSplit <-
		function (num) {
			x_( xSplit(num, self_()) )
		}
	this$xSplit... <-
		function (num, ...) {
			x_( xSplit...(num, self_(), ...) )
		}

	this$x_Split <-
		function (num) {
			xSplit(num, self_())
		}
	this$x_Split... <-
		function (num, ...) {
			xSplit...(num, self_(), ...)
		}

	# --- xShuffle --- #
	this$xShuffle <-
		function () {
			x_( xShuffle(self_()) )
		}
	this$xShuffle... <-
		function (...) {
			x_( xShuffle...(self_(), ...) )
		}

	this$x_Shuffle <-
		function () {
			xShuffle(self_())
		}
	this$x_Shuffle... <-
		function (...) {
			xShuffle...(self_(), ...)
		}

	# --- xSplitWith --- #
	this$xSplitWith <-
		function (pred) {
			x_( xSplitWith(pred, self_()) )
		}
	this$xSplitWith... <-
		function (pred, ...) {
			x_( xSplitWith...(pred, self_(), ...) )
		}

	this$x_SplitWith <-
		function (pred) {
			xSplitWith(pred, self_())
		}
	this$x_SplitWith... <-
		function (pred, ...) {
			xSplitWith...(pred, self_(), ...)
		}

	# --- xStopwatch --- #

	this$xStopwatch <-
		function () {
			x_( xStopwatch(self_()) )
		}

	this$x_Stopwatch <-
		function () {
			xStopwatch(self_())
		}

	# --- xSubString --- #
	this$xSubString <-
		function (nums) {
			x_( xSubString(self_(), nums) )
		}
	this$xSubString... <-
		function (...) {
			x_( xSubString...(self_(), ...) )
		}

	this$x_SubString <-
		function (nums) {
			xSubString(self_(), nums)
		}
	this$x_SubString... <-
		function (...) {
			xSubString...(self_(), ...)
		}

	# --- xSucc --- #
	this$xSucc <-
		function () {
			x_( xSucc(self_()) )
		}
	this$xSucc... <-
		function (...) {
			x_( xSucc...(self_(), ...) )
		}

	this$x_Succ <-
		function () {
			xSucc(self_())
		}
	this$x_Succ... <-
		function (...) {
			xSucc...(self_(), ...)
		}

	# -------- T ------- #
	# --- xTake --- #
	this$xTake <-
		function (num) {
			x_( xTake(num, self_()) )
		}
	this$xTake... <-
		function (num, ...) {
			x_( xTake...(num, self_(), ...) )
		}

	this$x_Take <-
		function (num) {
			xTake(num, self_())
		}
	this$x_Take... <-
		function (num, ...) {
			xTake...(num, self_(), ...)
		}

	# --- xTakeWhile --- #
	this$xTakeWhile <-
		function (pred) {
			x_( xTakeWhile(pred, self_()) )
		}
	this$xTakeWhile... <-
		function (pred, ...) {
			x_( xTakeWhile...(pred, self_(), ...) )
		}

	this$x_TakeWhile <-
		function (pred) {
			xTakeWhile(pred, self_())
		}
	this$x_TakeWhile... <-
		function (pred, ...) {
			xTakeWhile...(pred, self_(), ...)
		}

	# --- xThird --- #
	this$xThird <-
		function () {
			x_( xThird(self_()) )
		}
	this$xThird... <-
		function (...) {
			x_( xThird...(self_(), ...) )
		}

	this$x_Third <-
		function () {
			xThird(self_())
		}
	this$x_Third... <-
		function (...) {
			xThird...(self_(), ...)
		}

	# -------- U ------- #
	# --- xUnchars --- #

	this$xUnchars <-
		function () {
			x_( xUnchars(self_()) )
		}
	this$xUnchars... <-
		function (...) {
			x_( xUnchars...(self_(), ...) )
		}

	this$x_Unchars <-
		function () {
			xUnchars(self_())
		}
	this$x_Unchars... <-
		function (...) {
			xUnchars...(self_(), ...)
		}

	# --- xUnion --- #
	this$xUnion <-
		function () {
			x_( xUnion(self_()) )
		}

	this$x_Union <-
		function () {
			xUnion(self_())
		}

	this$xUnion... <-
		function (...) {
			x_( xUnion...(self_(), ...) )
		}

	this$x_Union... <-
		function (...) {
			xUnion...(self_(), ...)
		}

	# --- xUnit --- #
	this$xUnit <-
		function () {
			x_( xUnit(self_())	)
		}

	this$x_Unit <-
		function () {
			xUnit(self_())
		}

	# --- xUnlines --- #
	this$xUnlines <-
		function () {
			x_( xUnlines(self_()) )
		}
	this$xUnlines... <-
		function (...) {
			x_( xUnlines...(self_(), ...) )
		}

	this$x_Unlines <-
		function () {
			xUnlines(self_())
		}
	this$x_Unlines... <-
		function (...) {
			xUnlines...(self_(), ...)
		}

	# --- xUnwords --- #
	this$xUnwords <-
		function () {
			x_( xUnwords(self_()) )
		}
	this$xUnwords... <-
		function (...) {
			x_( xUnwords...(self_(), ...) )
		}

	this$x_Unwords <-
		function () {
			xUnwords(self_())
		}
	this$x_Unwords... <-
		function (...) {
			xUnwords...(self_(), ...)
		}

 	# -------- V ------- #
	# -------- W ------- #
	# --- xWords --- #
	this$xWords <-
		function () {
			x_( xWords(self_()) )
		}

	this$x_Words <-
		function () {
			xWords(self_())
		}

	this$xWait <-
		function (fn) {
			x_( xWait(fn, self_()) )
		}

	this$x_Wait <-
		function (fn) {
			xWait(fn, self_())
		}

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
	# --- xZip --- #
	this$xZip <-
		function () {
			x_( xZip(self_()) )
		}
	this$x_Zip <-
		function () {
			xZip(self_())
		}

	this$xZip... <-
		function (...) {
			x_( xZip...(self_(), ...) )
		}
	this$x_Zip... <-
		function (...) {
			xZip...(self_(), ...)
		}

	# --- xZipWith --- #
	this$xZipWith <-
		function (fn) {
			x_( xZipWith(fn, self_()) )
		}
	this$xZipWith... <-
		function (fn, ...) {
			x_( xZipWith(fn, self_(), ...) )
		}

	this$x_ZipWith <-
		function (fn) {
			xZipWith(fn, self_())
		}
	this$x_ZipWith... <-
		function (fn, ...) {
			xZipWith(fn, self_(), ...)
		}

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "collections"
	)
	this
})




























# -------------------------------- Function methods -------------------------------- #
#
# These methods operate on functions wrapped in the arrow object.
# I anticipate that these methods will be less used, but there's no reason to exclude them
# entirely. Methods such as Apply work nicely with this style.






x_fn_proto <- local({

	this <- Object()
	# -------- A ------- #
	# --- xAsClosure --- #
	this$xAsClosure <-
		function () {
			x_( xAsClosure(self_()) )
		}
	this$x_AsClosure <-
		function () {
			xAsClosure(self_())
		}

	# --- xAsUnary --- #
	this$xAsUnary <-
		function () {
			x_( xAsUnary(self_()) )
		}
	this$x_AsUnary <-
		function () {
			xAsUnary(self_())
		}

	# --- xAsVariadic --- #
	this$xAsVariadic <-
		function () {
			x_( xAsVariadic(self_()) )
		}
	this$x_AsVariadic <-
		function () {
			xAsVariadic(self_())
		}

	# --- xApply --- #
	this$xApply <-
		function (coll) {
			x_( xApply(self_(), coll) )
		}
	this$xApply... <-
		function (...) {
			x_( xApply...(self_(), ...) )
		}

	this$x_Apply <-
		function (coll) {
			x_( xApply(self_(), coll) )
		}
	this$x_Apply... <-
		function (...) {
			xApply...(self_(), ...)
		}

	# --- xArity --- #
	this$xArity <-
		function () {
			x_( xArity(self_()) )
		}
	this$x_Arity <-
		function () {
			xArity(self_())
		}
	# -------- B ------- #


	# -------- C ------- #
	# --- xConst --- #
	this$xConst <-
		function () {
			x_( xConst(self_()) )
		}

	this$x_Const <-
		function () {
			xConst(self_())
		}

	this$xCompose... <-
		function (...) {
			x_( xCompose(list(self_(), ...)) )
		}

	this$x_Compose... <-
		function (...) {
			xCompose(list(self_(), ...))
		}
	# -------- D ------- #

	this$xDropWhile <-
		function (coll) {
			x_( xDropWhile(self_(), coll) )
		}
	this$x_DropWhile <-
		function (coll) {
			xDropWhile(self_(), coll)
		}

	this$xDropWhile... <-
		function (...) {
			x_( xDropWhile...(self_(), ...) )
		}
	this$x_DropWhile... <-
		function (...) {
			xDropWhile...(self_(), ...)
		}

	this$xDo <-
		function (coll) {
			x_( xDo(self_(), coll) )
		}
	this$x_Do <-
		function (coll) {
			xDo(self_(), coll)
		}

	this$xDo... <-
		function (...) {
			x_( xDo...(self_(), ...) )
		}

	this$x_Do... <-
		function (...) {
			xDo...(self_(), ...)
		}

	# -------- E ------- #

	this$xExists <-
		function (colls) {
			x_( xExists(self_(), colls) )
		}
	this$xExists... <-
		function (...) {
			x_( xExists...(self_(), ...) )
		}

	this$x_Exists <-
		function (colls) {
			xExists(self_(), colls)
		}
	this$x_Exists... <-
		function (...) {
			xExists...(self_(), ...)
		}
	# -------- F ------- #

	this$xFilter <-
		function (coll) {
			x_( xFilter(self_(), coll) )
		}
	this$x_Filter <-
		function (coll) {
			xFilter(self_(), coll)
		}

	this$xFilter... <-
		function (...) {
			x_( xFilter...(self_(), ...) )
		}
	this$x_Filter... <-
		function (...) {
			xFilter...(self_(), ...)
		}

	this$xFlip <-
		function () {
			x_( xFlip(self_()) )
		}
	this$x_Flip <-
		function () {
			xFlip(self_())
		}

	# --- xFlatMap --- #
	this$xFlatMap <-
		function (coll) {
			x_( xFlatMap(self_(), coll) )
		}
	this$xFlatMap... <-
		function (...) {
			x_( xFlatMap...(self_(), ...) )
		}

	this$x_FlatMap <-
		function (coll) {
			xFlatMap(self_(), coll)
		}
	this$x_FlatMap... <-
		function (...) {
			xFlatMap...(self_(), ...)
		}

	# --- xForall --- #
	this$xForall <-
		function (colls) {
			x_( xForall(self_(), colls) )
		}
	this$xForall... <-
		function (...) {
			x_( xForall...(self_(), ...) )
		}

	this$x_Forall <-
		function (colls) {
			xForall(self_(), colls)
		}
	this$x_Forall... <-
		function (...) {
			xForall...(self_(), ...)
		}

	# --- xFold --- #
	this$xFoldl <-
		function (init, coll) {
			x_( xFoldl(self_(), init, coll) )
		}
	this$xFoldl... <-
		function (init, ...) {
			x_( xFoldl...(self_(), init, ...) )
		}

	this$x_Foldl <-
		function (init, coll) {
			xFoldl(self_(), init, coll)
		}
	this$x_Foldl... <-
		function (init, ...) {
			xFoldl...(self_(), init, ...)
		}

	this$xFold <-
		this$xFoldl
	this$xFold... <-
		this$xFoldl...

	this$x_Fold <-
		this$x_Foldl
	this$x_Fold... <-
		this$x_Foldl...

	# --- xFoldr --- #
	this$xFoldr <-
		function (init, coll) {
			x_( xFoldr(self_(), init, coll) )
		}
	this$xFoldr... <-
		function (init, ...) {
			x_( xFoldr...(self_(), init, ...) )
		}

	this$x_Foldr <-
		function (init, coll) {
			xFoldr(self_(), init, coll)
		}
	this$x_Foldr... <-
		function (init, ...) {
			xFoldr...(self_(), init, ...)
		}

	# --- xFoldListl --- #
	this$xFoldListl <-
		function (init, coll) {
			x_( xFoldListl(self_(), init, coll) )
		}
	this$xFoldListl... <-
		function (init, ...) {
			x_( xFoldListl...(self_(), init, ...) )
		}

	this$x_FoldListl <-
		function (init, coll) {
			xFoldListl(self_(), init, coll)
		}
	this$x_FoldListl... <-
		function (init, ...) {
			xFoldListl...(self_(), init, ...)
		}

	this$xFoldList <-
		function (init, coll) {
			x_( xFoldList(self_(), init, coll) )
		}
	this$xFoldList... <-
		function (init, ...) {
			x_( xFoldList...(self_(), init, ...) )
		}

	this$x_FoldList <-
		function (init, coll) {
			xFoldList(self_(), init, coll)
		}
	this$x_FoldList... <-
		function (init, ...) {
			xFoldList...(self_(), init, ...)
		}

	# --- xFormals --- #
	this$xFormals <-
		function () {
			x_( xFormals(self_()) )
		}

	this$x_Formals <-
		function () {
			xFormals(self_())
		}

	# -------- G ------- #

	# -------- H ------- #
	# -------- I ------- #
	# --- xIsVariadic --- #
	this$xIsVariadic <-
		function () {
			x_( xIsVariadic(self_()) )
		}

	this$x_IsVariadic <-
		function () {
			xIsVariadic(self_())
		}
	# --- xIterate --- #
	this$xIterate <-
		function (init) {
			x_( xIterate(self_(), init ) )
		}

	this$x_Iterate <-
		function (init) {
			xIterate(self_(), init )
		}
	# -------- J ------- #
	# --- xJuxtapose --- #
	this$xJuxtapose... <-
		function (...) {
			x_( xJuxtapose...(self_(), ...) )
		}

	this$x_Juxtapose... <-
		function (...) {
			xJuxtapose...(self_(), ...)
		}

	# -------- K ------- #
	# --- xK --- #
	this$xK <-
		this$xConst
	this$x_K <-
		this$x_Const

	# -------- L ------- #
	# --- xLimit --- #
	this$xLimit <-
		function (num) {
			x_( xLimit(self_(), num) )
		}

	this$x_Limit <-
		function (num) {
			xLimit(self_(), num)
		}

	# --- xLocate --- #

	this$xLift <-
		function (fns) {
			x_( xLift(self_(), fns) )
		}
	this$x_Lift <-
		function (fns) {
			xLift(self_(), fns)
		}

	this$xLift... <-
		function (...) {
			x_( xLift(self_(), list(...)) )
		}
	this$x_Lift... <-
		function (...) {
			xLift(self_(), list(...))
		}

	# --- xLocatel --- #
	this$xLocatel <-
		function (coll) {
			x_( xLocatel(self_(), coll) )
		}
	this$xLocatel... <-
		function (...) {
			x_( xLocatel...(self_(), ...) )
		}

	this$x_Locatel <-
		function (coll) {
			xLocatel(self_(), coll)
		}
	this$x_Locatel... <-
		function (...) {
			xLocatel...(self_(), ...)
		}

	this$xLocate <-
		this$xLocatel
	this$xLocate... <-
		this$xLocatel...

	this$x_Locate <-
		this$x_Locatel
	this$x_Locate... <-
		this$x_Locatel...

	# --- xLocater --- #
	this$xLocater <-
		function (coll) {
			x_( xLocater(self_(), coll) )
		}
	this$xLocater... <-
		function (...) {
			x_( xLocater...(self_(), ...) )
		}

	this$x_Locater <-
		function (coll) {
			xLocater(self_(), coll)
		}
	this$x_Locater... <-
		function (...) {
			xLocater...(self_(), ...)
		}

	# -------- M ------- #
	# --- xMap --- #
	this$xMap <-
		function (coll) {
			x_( xMap(self_(), coll) )
		}
	this$xMap... <-
		function (...) {
			x_( xMap...(self_(), ...) )
		}

	this$x_Map <-
		function (coll) {
			xMap(self_(), coll)
		}
	this$x_Map... <-
		function (...) {
			xMap...(self_(), ...)
		}

	# --- xMapply --- #
	this$xMapply <-
		function (coll) {
			x_( xMapply(self_(), coll) )
		}
	this$xMapply... <-
		function (...) {
			x_( xMapply...(self_(), ...) )
		}

	this$x_Mapply <-
		function (coll) {
			xMapply(self_(), coll)
		}
	this$x_Mapply... <-
		function (...) {
			xMapply...(self_(), ...)
		}

	# --- xMapIndexed --- #
	this$xMapIndexed <-
		function (coll) {
			x_( xMapIndexed(self_(), coll) )
		}
	this$xMapIndexed... <-
		function (...) {
			x_( xMapIndexed...(self_(), ...) )
		}

	this$x_MapIndexed <-
		function (coll) {
			xMapIndexed(self_(), coll)
		}
	this$x_MapIndexed... <-
		function (...) {
			xMapIndexed...(self_(), ...)
		}

	# --- xMapMany --- #
	this$xMapMany <-
		function (colls) {
			x_( xMapMany(self_(), colls) )
		}
	this$xMapMany... <-
		function (...) {
			x_( xMapMany...(self_(), ...) )
		}

	this$x_MapMany <-
		function (colls) {
			xMapMany(self_(), colls)
		}
	this$x_MapMany... <-
		function (...) {
			xMapMany...(self_(), ...)
		}

	# --- xMapWhen --- #
	this$xMapWhen <-
		function (fn, coll) {
			x_( xMapWhen(self_(), fn, coll) )
		}
	this$xMapWhen... <-
		function (fn, ...) {
			x_( xMapWhen...(self_(), fn, ...) )
		}

	this$x_MapWhen <-
		function (fn, coll) {
			xMapWhen(self_(), fn, coll)
		}
	this$x_MapWhen... <-
		function (fn, ...) {
			xMapWhen...(self_(), fn, ...)
		}

	# -------- N ------- #
	this$xNot <-
		function () {
			x_( xNot(self_()) )
		}
	this$x_Not <-
		function () {
			xNot(self_())
		}

	# -------- O ------- #

	# -------- P ------- #
	# --- xPartition --- #
	this$xPartition <-
		function (coll) {
			x_( xPartition(self_(), coll) )
		}
	this$xPartition... <-
		function (...) {
			x_( xPartition...(self_(), ...) )
		}

	this$x_Partition <-
		function (coll) {
			xPartition(self_(), coll)
		}
	this$x_Partition... <-
		function (...) {
			xPartition...(self_(), ...)
		}

	# --- xParams --- #
	this$xParams <-
		function () {
			x_( xParams(self_()) )
		}

	this$x_Params <-
		function () {
			xParams(self_())
		}

	# --- xPartial --- #
	this$xPartial <-
		function (coll) {
			x_( xPartial(self_(), coll) )
		}
	this$xPartial... <-
		function (...) {
			x_( xPartial...(self_(), ...) )
		}

	this$x_Partial <-
		function (coll) {
			xPartial(self_(), coll)
		}
	this$x_Partial... <-
		function (...) {
			xPartial...(self_(), ...)
		}

	# --- xPoll --- #
	this$xPoll <-
		function (coll) {
			x_( xPoll(self_(), coll) )
		}
	this$xPoll... <-
		function (...) {
			x_( xPoll...(self_(), ...) )
		}

	this$x_Poll <-
		function (coll) {
			xPoll(self_(), coll)
		}
	this$x_Poll... <-
		function (...) {
			xPoll...(self_(), ...)
		}

	# -------- Q ------- #
	# -------- R ------- #
	# --- xRecurMap --- #
	this$xRecurMap <-
		function (coll) {
			x_( xRecurMap(self_(), coll) )
		}
	this$xRecurMap... <-
		function (...) {
			x_( xRecurMap...(self_(), ...) )
		}

	this$x_RecurMap <-
		function (coll) {
			xRecurMap(self_(), coll)
		}
	this$x_RecurMap... <-
		function (...) {
			xRecurMap...(self_(), ...)
		}

	# --- xReducel --- #
	this$xReducel <-
		function (coll) {
			x_( xReducel(self_(), coll) )
		}
	this$xReducel... <-
		function (...) {
			x_( xReducel...(self_(), ...) )
		}

	this$x_Reducel <-
		function (coll) {
			xReducel(self_(), coll)
		}
	this$x_Reducel... <-
		function (...) {
			xReducel...(self_(), ...)
		}

	this$xReduce <-
		this$xReducel
	this$xReduce... <-
		this$xReducel...

	this$x_Reduce <-
		this$x_Reducel
	this$x_Reduce... <-
		this$x_Reducel...

	# --- xReducer --- #
	this$xReducer <-
		function (coll) {
			x_( xReducer(self_(), coll) )
		}
	this$xReducer... <-
		function (...) {
			x_( xReducer...(self_(), ...) )
		}

	this$x_Reducer <-
		function (coll) {
			xReducer(self_(), coll)
		}
	this$x_Reducer... <-
		function (...) {
			xReducer...(self_(), ...)
		}

	# --- xReject --- #
	this$xReject <-
		function (coll) {
			x_( xReject(self_(), coll) )
		}
	this$xReject... <-
		function (...) {
			x_( xReject...(self_(), ...) )
		}

	this$x_Reject <-
		function (coll) {
			xReject(self_(), coll)
		}
	this$x_Reject... <-
		function (...) {
			xReject...(self_(), ...)
		}
	# -------- S ------- #
	# --- xSelect --- #
	this$xSelect <-
		function (coll) {
			x_( xSelect(self_(), coll) )
		}
	this$xSelect... <-
		function (...) {
			x_( xSelect...(self_(), ...) )
		}

	this$x_Select <-
		function (coll) {
			xSelect(self_(), coll)
		}
	this$x_Select... <-
		function (...) {
			xSelect...(self_(), ...)
		}

	# --- xSplitWith --- #
	this$xSplitWith <-
		function (coll) {
			x_( xSplitWith(self_(), coll) )
		}
	this$xSplitWith... <-
		function (...) {
			x_( xSplitWith...(self_(), ...) )
		}

	this$x_SplitWith <-
		function (coll) {
			xSplitWith(self_(), coll)
		}
	this$x_SplitWith... <-
		function (...) {
			xSplitWith...(self_(), ...)
		}
	# -------- T ------- #
	# --- xTap --- #
	this$xTap <-
		function (fn) {
			x_( fn(self_()) )
		}

	this$x_Tap <-
		function (fn) {
			fn(self_())
		}

	# --- xTakeWhile --- #
	this$xTakeWhile <-
		function (coll) {
			x_( xTakeWhile(self_(), coll) )
		}
	this$xTakeWhile... <-
		function (...) {
			x_( xTakeWhile...(self_(), ...) )
		}

	this$x_TakeWhile <-
		function (coll) {
			xTakeWhile(self_(), coll)
		}
	this$x_TakeWhile... <-
		function (...) {
			xTakeWhile...(self_(), ...)
		}

	# --- xT --- #
	this$xT <-
		this$xThrush
	this$x_T <-
		this$xThrush
	# -------- U ------- #
	# -------- V ------- #
	# --- xVectorise --- #
	this$xVectorise <-
		function () {
			x_( xVectorise(self_()) )
		}

	this$x_Vectorise <-
		function () {
			xVectorise(self_())
		}

	this$xVectorize <-
		function () {
			x_( xVectorize(self_()) )
		}

	this$x_Vectorize <-
		function () {
			xVectorize(self_())
		}
	# -------- W ------- #

	# --- xWait --- #
	this$xWait <-
		function (num) {
			x_( xWait(self_(), num) )
		}

	this$x_Wait <-
		function (num) {
			xWait(self_(), num)
		}
	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #

	# --- xZipWith --- #
	this$xZipWith <-
		function (colls) {
			x_( xZipWith(self_(), colls) )
		}
	this$xZipWith... <-
		function (...) {
			x_( xZipWith...(self_(), ...) )
		}

	this$x_ZipWith <-
		function (colls) {
			xZipWith(self_(), colls)
		}
	this$x_ZipWith... <-
		function (...) {
			xZipWith...(self_(), ...)
		}

	# --- xZip --- #
	this$xZip <-
		function () {
			x_( xZip(self_()) )
		}
	this$xZip... <-
		function (...) {
			x_( xZip...(self_(), ...) )
		}

	this$x_Zip <-
		function () {
			xZip(self_())
		}
	this$x_Zip... <-
		function (...) {
			xZip...(self_(), ...)
		}

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "functions"
	)
	this
})






# -------------------------------- Type Constructor -------------------------------- #

#' x_
#'
#' Generate a chainable arrow object that methods can be called off.
#'
#' @param
#'    val a function, collection, or arbitrary value.
#'
#' @return
#'    an object of class "arrow", with a single field 'x' that contents_are val.
#'
#' @section Corner Cases:
#'    The methods that can be used by x_() object varies depending on the type of val.
#'    Some methods are specific to functions or collections. If a non-function and non-collection is
#'    supplied then very few methods can be used.
#'
#'    Because the definition of $ was overloaded to allow method chaining, the
#'    field 'x' inside an arrow object cannot be accessed using x_()$x. Writing
#'    x_()$x() is required.
#'
#' @details
#'    The arrow object constructed by \code{x_} contains methods specific to the
#'    the type of data given to the constructor. As of Arrow 0.1.0, there are
#'    the following sets of methods:
#'
#'    1. x_([any])
#'
#'    Methods that are added to every data type.
#'
#'    \code{xExecute}: .
#'
#'    \code{xGraft}: Make a new method available for use on arrow objects.
#'
#'    \code{xTap}: Call the data inside the arrow object with an anonymous function, returning
#'    a new arrow object.
#'
#' @export

x_ <- function (val) {
	# Collection any -> Arrow any
	# type constructor for the method-chaining data type.

	invoking_call <- sys.call()

	assert(
		!missing(val), invoking_call,
		exclaim$parametre_missing(val))

	if ('arrow' %in% class(val)) {
		val
	} else {
		structure(list(x = val), class = 'arrow')
	}
}

get_proto_ref <- function (val) {
	# get the reference to the appropriate methods.

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

#' @method $ arrow

'$.arrow' <- local({

	suggest_similar_method <- function (val, method_name, contents_are, invoking_call) {
		# given an incorrect method name throw an error
		# suggesting a similar

		proto_ref <- get_proto_ref(val)
		method_name <- method_name

		candidate_methds <- setdiff(ls(proto_ref), 'private')
		distances <- adist(method_name, candidate_methds)

		similar <- if (min(distances) < nchar(method_name) / 2) {

			candidate_methds[which.min(distances)]

		} else {
			character(0)
		}

		stop(
			exclaim$method_not_found(
				method_name, contents_are, similar),
			call. = False)
	}

	function (obj, method) {
		# Arrow a -> symbol -> function
		# return an arrow method associated with the type a.

		method_name <- paste0(method)
		invoking_call <- paste0('$', method_name)

		proto_ref <- get_proto_ref( obj[['x']] )

		if (!method_name %in% ls(proto_ref) || method_name == "private") {
			# the invoked method wasn't found,
			# so we should give a suggestion.

			contents_are <- proto_ref[['private']][['contents_are']]

			suggest_similar_method(
				obj[['x']], method_name, contents_are, invoking_call)

		}

		fn <- proto_ref[[method_name]]
		environment(fn)[['self_']] <- function () obj[['x']]
		fn
	}
})

# -------------------------------- Print Method -------------------------------- #

#' @method print arrow

print.arrow <- function (x, ...) {
	# custom print statement for the arrow object.

	proto_ref <- get_proto_ref( x[['x']] )
	contents_are <- proto_ref[['private']] [['contents_are']]

	single_newline <- '\n'
	double_newline <- '\n\n'

	cat(
		'[ an arrow object with methods for ' %+%
			contents_are %+%
		' ]'  %+% double_newline %+%
		'$x()' %+% single_newline)

	print(x$x())
}
