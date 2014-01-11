

# -------------------------------- x_( ) -------------------------------- #
#
# The x_() function is a constructor that wraps a datum, and allows methods
# to be called on that datum. The constructor returns a monad, so the
# methods can be chained indefinetly until the $x_() method is used to pull the
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











# -------------------------------- Method Creators -------------------------------- #
#
# I don't like creating code dynamically, but it will reduce the amount of Arrow
# code to 1/3 of writing it by hand.

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

xMethod <- function (fn, fixed) {
	# generate the xMethod form of the function.

	invoking_frame <- parent.frame()

	fixed_sym <- as.symbol(match.call()$fixed)
	fixed_name <- as.symbol(match.call()$fixed)

	fn_sym <- as.symbol(match.call()$fn)

	if (!(fixed %in% names(formals(fn)) )) {
		stop('not a parametre of ' %+% paste0(fn_sym))
	}

	method <- function () {

	}

	formals(method) <-
		formals(fn)[ names(formals(fn)) != fixed ]

	body(method) <-
		bquote({

		x_(.(
			( as.call(c(
				fn_sym,
				lapply(
					xParamsOf(fn),
					function (param) {

						if (as.symbol(param) == fixed) {
							quote(self_())
						} else {
							as.symbol(param)
						}
					}) )) ) ))
	})

	environment(method) <- invoking_frame
	method
}

x_Method <- function (fn, fixed) {
	# generate the x_Method form of the function.

	invoking_frame <- parent.frame()

	fixed_sym <- as.symbol(match.call()$fixed)
	fixed_name <- as.symbol(match.call()$fixed)

	fn_sym <- as.symbol(match.call()$fn)

	if (!(fixed %in% names(formals(fn)) )) {
		stop('not a parametre of fn')
	}

	method <- function () {

	}

	formals(method) <-
		formals(fn)[ names(formals(fn)) != fixed ]

	body(method) <-
		bquote({

		.(
			( as.call(c(
				fn_sym,
				lapply(
					xParamsOf(fn),
					function (param) {

						if (as.symbol(param) == fixed) {
							quote(self_())
						} else {
							as.symbol(param)
						}
					}) )) ))
	})

	environment(method) <- invoking_frame
	method
}


















# -------------------------------- Universal methods -------------------------------- #
#
# these prototypes contain methods that can be called by an x_() object, using an
# overloaded definition of the $ function.

x_any_proto <- local({

	this <- Object()

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

	this$x_Tap <-
		function (fn) {

			fn(self_())
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
	this$x_ <-
		function () {
			self_()
		}
	# -------- Y ------- #

	# -------- Z ------- #

	this$private <- list(
		contents_are = "arbitrary values")
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

	this$xByColnames <-
		function () {
			x_( as.list( colnames(self_()) ) )
		}
	this$x_ByColnames <-
		function () {
			as.list( colnames(self_()) )
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
	# --- xByRownames --- #
		this$xByRownames <-
			function () {
				x_( as.list( rownames(self_()) ) )
			}

		this$x_ByRownames <-
			function () {
				as.list( rownames(self_()) )
			}

	# -------- C ------- #
	this$xColUnit <-
		function () {
			x_(matrix(
				nrow = nrow(self_()),
				ncol = 0))
		}
	this$x_ColUnit <-
		function () {
			matrix(
				nrow = nrow(self_()),
				ncol = 0)
		}

	# -------- D ------- #

	# -------- E ------- #
	this$xElemsByCols <-
		function () {
			if (prod(dim(self_()) == 0)) {
				x_( list() )
			} else {
				x_( as.list(self_()) )
			}
		}
	this$x_ElemsByCols <-
		function () {
			if (prod(dim(self_()) == 0)) {
				list()
			} else {
				as.list(self_())
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
	# --- xRowUnit --- #
	this$xRowUnit <-
		function () {
			x_(matrix(
				nrow = 0,
				ncol = ncol(self_()) ))
		}
	this$x_RowUnit <-
		function () {
			matrix(
				nrow = 0,
				ncol = ncol(self_()) )
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
		contents_are = "matrices")

	this
})






x_data_frame_proto <- local({

	this <- Object()

	# -------- A ------- #

	# -------- B ------- #
	# --- xByCols --- #
	this$xByCols <-
		function () {
			x_(unname( as.list(self_()) ))
		}
	this$x_ByCols <-
		function () {
			unname( as.list(self_()) )
		}
	# --- xByColnames --- #
	this$xByColnames <-
		function () {
			x_( as.list( colnames(self_()) ) )
		}
	this$x_ByColnames <-
		function () {
			as.list( colnames(self_()) )
		}

	# --- xByRownames --- #
	this$xByRownames <-
		function () {
			x_( as.list( rownames(self_()) ) )
		}
	this$x_ByRownames <-
		function () {
			as.list( rownames(self_()) )
		}

	# -------- C ------- #
	# --- xColUnit --- #
	this$xColUnit <-
		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = nrow(self_()),
					ncol = 0)) ))
		}
	this$x_ColUnit <-
		function () {
			unname(as.data.frame(
				matrix(
					nrow = nrow(self_()),
					ncol = 0)) )
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
	# --- xRowUnit --- #
	this$xRowUnit <-
		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = ncol(self_()) )) ))
		}
	this$x_RowUnit <-
		function () {
			unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = ncol(self_()) )) )
		}

	# -------- S ------- #

	# -------- T ------- #

	# -------- U ------- #
	# --- xUnit --- #
	this$xUnit <-
		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = 0 )) ))
		}
	this$x_Unit <-
		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = 0 )) ))
		}

	# -------- V ------- #

	# -------- W ------- #

	# -------- X ------- #

	# -------- Y ------- #

	# -------- Z ------- #

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "data.frames")

	this
})


x_factor_proto <- local({

	this <- Object()

	# -------- A ------- #

	# -------- B ------- #

	this$xByLevels <-
		function () {
			x_( as.character( levels(self_()) ) )
		}

	this$x_ByLevels <-
		function () {
			as.character( levels(self_()) )
		}


	this$xByValues <-
		function () {
			x_( as.vector(self_()) )
		}

	this$x_ByValues <-
		function () {
			as.vector(self_())
		}

	# -------- C ------- #

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

	# -------- S ------- #

	# -------- T ------- #

	# -------- U ------- #

	# -------- V ------- #

	# -------- W ------- #

	# -------- X ------- #

	# -------- Y ------- #

	# -------- Z ------- #

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "factors")

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

	# --- xAsLogical --- #
	this$xAsLogical <-
		xMethod(xAsLogical, 'bools')

	this$xAsLogical... <-
		function (...) {
			x_( xAsLogical(self_(), ...) )
		}

	this$x_AsLogical <-
		x_Method(xAsLogical, 'bools')

	this$x_AsLogical... <-
		function (...) {
			xAsLogical(self_(), ...)
		}

	# --- xAsInteger --- #
	this$xAsInteger <-
		xMethod(xAsInteger, 'nums')

	this$xAsInteger... <-
		function (...) {
			x_( xAsInteger(self_(), ...) )
		}

	this$x_AsInteger <-
		x_Method(xAsInteger, 'nums')

	this$x_AsInteger... <-
		function (...) {
			xAsInteger(self_(), ...)
		}

	# --- xAsCharacter --- #
	this$xAsCharacter <-
		xMethod(xAsCharacter, 'strs')

	this$xAsCharacter... <-
		function (...) {
			x_( xAsCharacter(self_(), ...) )
		}

	this$x_AsCharacter <-
		x_Method(xAsCharacter, 'strs')

	this$x_AsCharacter... <-
		function (...) {
			xAsCharacter(self_(), ...)
		}

	# --- xAsDouble --- #
	this$xAsDouble <-
		xMethod(xAsDouble, 'nums')

	this$xAsDouble... <-
		function (...) {
			x_( xAsDouble(self_(), ...) )
		}

	this$x_AsDouble <-
		x_Method(xAsDouble, 'nums')

	this$x_AsDouble... <-
		function (...) {
			xAsDouble(self_(), ...)
		}

	# --- xAsRaw --- #
	this$xAsRaw <-
		xMethod(xAsRaw, 'raws')

	this$xAsRaw... <-
		function (...) {
			x_( xAsRaw(self_(), ...) )
		}

	this$x_AsRaw <-
		x_Method(xAsRaw, 'raws')

	this$x_AsRaw... <-
		function (...) {
			xAsRaw(list(...))
		}

	# --- xAsComplex --- #
	this$xAsComplex <-
		xMethod(xAsComplex, 'comps')

	this$xAsComplex... <-
		function (...) {
			x_( xAsComplex(self_(), ...) )
		}

	this$x_AsComplex <-
		x_Method(xAsComplex, 'comps')

	this$x_AsComplex... <-
		function (...) {
			xAsComplex(self_(), ...)
		}

	# --- xAsFunction --- #
#	this$xAsFunction <-
#		xMethod(xAsFunction, 'coll')

#	this$xAsFunction... <-
#		function (...) {
#			x_(  xAsFunction...(self_(), ...) )
#		}

#	this$x_AsFunction <-
#		x_Method(xAsFunction, 'coll')

#	this$x_AsFunction... <-
#		function (...) {
#			 xAsFunction...(self_(), ...)
#		}

	# --- xApply --- #
	this$xApply <-
		xMethod(xApply, 'coll')

	this$xApply... <-
		function (fn, ...) {
			x_( xApply...(fn, self_(), ...) )
		}
	this$x_Apply <-
		x_Method(xApply, 'coll')

	this$x_Apply... <-
		function (fn, ...) {
			xApply...(fn, self_(), ...)
		}

	# --- xAssoc --- #
	this$xAssoc <-
		xMethod(xAssoc, 'colls')

	this$xAssoc... <-
		function (...) {
			x_( xAssoc...(self_(), ...) )
		}

	this$x_Assoc <-
		x_Method(xAssoc, 'colls')

	this$x_Assoc... <-
		function (...) {
			xAssoc...(self_(), ...)
		}

	# -------- B ------- #
	# -------- C ------- #

	# --- xToChars --- #
	this$xToChars <-
		function () {
			x_( xToChars(self_()) )
		}
	this$x_ToChars <-
		function () {
			xToChars(self_())
		}

	# --- xChop --- #

	this$xChop <-
		xMethod(xChop, 'coll')
	this$xChop... <-
		function (num, ...) {
			x_( xChop(num, self_(), ...) )
		}

	this$x_Chop <-
		function (num) {
			xChop(num, self_())
		}
	this$x_Chop... <-
		function (num, ...) {
			xChop(num, self_(), ...)
		}

	# --- xCombos --- #
	this$xCombos <-
		xMethod(xCombos, 'coll')
	this$xCombos... <-
		function (num, ...) {
			x_( xCombos...(num, self_(), ...) )
		}

	this$x_Combos <-
		function (num) {
			xCombos(num, self_())
		}
	this$x_Combos... <-
		function (num, ...) {
			xCombos...(num, self_(), ...)
		}

	# --- xConst --- #
	this$xConst <-
		xMethod(xConst, 'val')

	this$x_Const <-
		function () {
			xConst(self_())
		}

	# -------- D ------- #
	# --- xDissoc --- #
	this$xDissoc <-
		xMethod(xDissoc, 'colls')

	this$xDissoc... <-
		function (...) {
			x_( xDissoc(self_(), ...) )
		}

	this$x_Dissoc <-
		function () {
			xDissoc(self_())
		}
	this$x_Dissoc... <-
		function (...) {
			xDissoc(self_(), ...)
		}

	# --- xDiffer --- #
	this$xDiffer <-
		xMethod(xDiffer, 'colls')

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
		xMethod(xDrop, 'coll')

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
		xMethod(xDo, 'coll')
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
		xMethod(xDropWhile, 'coll')
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
	# --- xDuplicated --- #
	this$xDuplicated <-
		xMethod(xDuplicated, 'coll')

	this$x_Duplicated <-
		function (coll) {
			xDuplicated(self_())
		}

	this$xDuplicated... <-
		function (...) {
			x_( xDuplicated(self_(), ...) )
		}

	this$x_Duplicated... <-
		function (...) {
			xDuplicated(self_(), ...)
		}

	# -------- E ------- #

	# --- xExists --- #
	this$xExists <-
		xMethod(xExists, 'colls')
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
		xMethod(xExplode, 'str')
	this$x_Explode <-
		function (rexp) {
			xExplode(rexp, self_())
		}

	# -------- F ------- #

	# --- xFirst --- #
	this$xFirst <-
		xMethod(xFirst, 'coll')
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

	# --- xFoldl --- #
	this$xFoldl <-
		xMethod(xFoldl, 'coll')

	this$xFoldl... <-
		function (fn, val, ...) {
			x_( xFoldl...(fn, val, self_(), ...) )
		}

	this$x_Foldl <-
		function (fn, val) {
			xFoldl(fn, val, self_())
		}
	this$x_Foldl... <-
		function (fn, val, ...) {
			xFoldl...(fn, val, self_(), ...)
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
		xMethod(xFlatMap, 'coll')
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
		xMethod(xFlatten, 'coll')

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
		xMethod(xForall, 'colls')

	this$xForall... <-
		function (pred, ...) {
			x_( xForall...(pred, self_(), ...) )
		}

	this$x_Forall <-
		x_Method(xForall, 'colls')

	this$x_Forall... <-
		function (pred, ...) {
			xForall...(pred, self_(), ...)
		}

	# --- xFoldr --- #
	this$xFoldr <-
		xMethod(xFoldr, 'coll')

	this$xFoldr... <-
		function (fn, val, ...) {
			x_( xFoldr...(fn, val, self_(), ...) )
		}

	this$x_Foldr <-
		function (fn, val) {
			xFoldr(fn, val, self_())
		}
	this$x_Foldr... <-
		function (fn, val, ...) {
			xFoldr...(fn, val, self_(), ...)
		}

	# --- xFoldListl --- #
	this$xFoldListl <-
		xMethod(xFoldListl, 'coll')

	this$xFoldListl... <-
		function (fn, val, ...) {
			x_( xFoldListl...(fn, val, self_(), ...) )
		}

	this$x_FoldListl <-
		function (fn, val) {
			xFoldListl(fn, val, self_())
		}
	this$x_FoldListl... <-
		function (fn, val, ...) {
			xFoldListl...(fn, val, self_(), ...)
		}

	this$xFoldList <-
		function (fn, val) {
			x_( xFoldList(fn, val, self_()) )
		}
	this$xFoldList... <-
		function (fn, val, ...) {
			x_( xFoldList...(fn, val, self_(), ...) )
		}

	this$x_FoldList <-
		function (fn, val) {
			xFoldList(fn, val, self_())
		}
	this$x_FoldList... <-
		function (fn, val, ...) {
			xFoldList...(fn, val, self_(), ...)
		}

	# --- xFourth --- #
	this$xFourth <-
		xMethod(xFourth, 'coll')

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
	this$xGetKey <-
		xMethod(xGetKey, 'str')

	this$x_GetKey <-
		function () {
			xGetKey(self_())
		}

	# -------- H ------- #
	# -------- I ------- #

	# --- xImplode --- #
	this$xImplode <-
		xMethod(xImplode, 'strs')

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
		xMethod(xIsMember, 'coll')

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
		xMethod(xInit, 'coll')

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
		xMethod(xIsEmpty, 'coll')

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
		xMethod(xIsFalse, 'coll')

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
		xMethod(xIsTrue, 'coll')

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
		xMethod(xIsNan, 'coll')

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
		xMethod(xIsNa, 'coll')

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
		xMethod(xIsNull, 'coll')

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
		xMethod(xIterate, 'val')

	this$x_Iterate <-
		function (fn) {
			xIterate(fn, self_())
		}

	# --- xInter --- #
	this$xInter <-
		xMethod(xInter, 'colls')

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
		xMethod(xJoin, 'colls')

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
		xMethod(xJuxtapose, 'fns')

	this$x_Juxtapose <-
		function (fns) {
			xJuxtapose(self_())
		}
	# -------- K ------- #
	# -------- L ------- #
	# --- xLast --- #
	this$xLast <-
		xMethod(xLast, 'coll')

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

	# --- xLenOf --- #
	this$xLenOf <-
		xMethod(xLenOf, 'coll')

	this$xLenOf... <-
		function (...) {
			x_( xLenOf...(self_(), ...) )
		}

	this$x_LenOf <-
		function () {
			xLenOf(self_())
		}
	this$x_LenOf... <-
		function (...) {
			xLenOf...(self_(), ...)
		}
	# --- xLimit --- #
	this$xLimit <-
		xMethod(xLimit, 'num')

	this$x_Limit <-
		function (fn) {
			xLimit(fn, self_())
		}

	# --- xToLines --- #
	this$xToLines <-
		xMethod(xToLines, 'str')

	this$x_ToLines <-
		function () {
			xToLines(self_())
		}

	# --- xLocatel --- #
	this$xLocatel <-
		xMethod(xLocatel, 'coll')

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
		xMethod(xLocater, 'coll')

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
		xMethod(xMap, 'coll')

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
		xMethod(xMapply, 'coll')

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
		xMethod(xMapIndexed, 'coll')

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
		xMethod(xMapMany, 'colls')

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

	# -------- N ------- #
	# --- xAsNamed --- #
	this$xAsNamed <-
		xMethod(xAsNamed, 'coll')

	this$x_AsNamed <-
		function (strs) {
			xAsNamed(strs, coll = self_())
		}

	# --- xNegate --- #
	this$xNegate <-
		xMethod(xNegate, 'nums')

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
		xMethod(xNotFalse, 'coll')

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
		xMethod(xNotTrue, 'coll')

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
		xMethod(xNotNa, 'coll')

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
		xMethod(xNotNan, 'coll')

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
		xMethod(xPack, 'coll')

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
		xMethod(xPoll, 'coll')

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
		xMethod(xPartial, 'coll')

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
		xMethod(xPluck, 'coll')

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
		xMethod(xPartition, 'coll')

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
		xMethod(xPermute, 'colls')

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
		xMethod(xPred, 'nums')

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
	# --- xDeepMap --- #
	this$xDeepMap <-
		xMethod(xDeepMap, 'coll')

	this$xDeepMap... <-
		function (fn, ...) {
			x_( xDeepMap...(fn, self_(), ...) )
		}

	this$x_RecurMap <-
		function (fn) {
			xDeepMap(fn, self_())
		}
	this$x_RecurMap... <-
		function (fn, ...) {
			xDeepMap...(fn, self_(), ...)
		}

	# --- xReducel --- #
	this$xReducel <-
		xMethod(xReducel, 'coll')

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
		xMethod(xReducer, 'coll')

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
		xMethod(xRepeat, 'coll')

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
		xMethod(xReject, 'coll')

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
		xMethod(xRest, 'coll')

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

	# --- xReverse --- #
	this$xReverse <-
		xMethod(xReverse, 'coll')

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
		xMethod(xSecond, 'coll')

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
		xMethod(xSetProd, 'colls')

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

	# --- xGroup --- #
	this$xGroup <-
		xMethod(xGroup, 'coll')

	this$xGroup... <-
		function (num, ...) {
			x_( xGroup...(num, self_(), ...) )
		}

	this$x_Group <-
		function (num) {
			xGroup(num, self_())
		}
	this$x_Group... <-
		function (num, ...) {
			xGroup...(num, self_(), ...)
		}

	# --- xSelect --- #
	this$xSelect <-
		xMethod(xSelect, 'coll')

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
		xMethod(xSignum, 'nums')

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

	# --- xSplitAt--- #
	this$xSplitAt<-
		xMethod(xSplitAt, 'coll')

	this$xSplitAt... <-
		function (num, ...) {
			x_( xSplitAt...(num, self_(), ...) )
		}

	this$x_SplitAt <-
		function (num) {
			xSplitAt(num, self_())
		}
	this$x_SplitAt... <-
		function (num, ...) {
			xSplitAt...(num, self_(), ...)
		}

	# --- xShuffle --- #
	this$xShuffle <-
		xMethod(xShuffle, 'coll')

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
		xMethod(xSplitWith, 'coll')

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
		xMethod(xStopwatch, 'num')

	this$x_Stopwatch <-
		function () {
			xStopwatch(self_())
		}

	# --- xSubstring --- #
	this$xSubstring <-
		xMethod(xSubstring, 'str')

	this$xSubstring... <-
		function (...) {
			x_( xSubstring...(self_(), ...) )
		}

	this$x_SubString <-
		function (nums) {
			xSubstring(self_(), nums)
		}
	this$x_SubString... <-
		function (...) {
			xSubstring...(self_(), ...)
		}

	# --- xSucc --- #
	this$xSucc <-
		xMethod(xSucc, 'nums')

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
		xMethod(xTake, 'coll')

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
		xMethod(xTakeWhile, 'coll')

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
		xMethod(xThird, 'coll')

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
	# --- xFromChars --- #

	this$xFromChars <-
		xMethod(xFromChars, 'strs')

	this$xFromChars... <-
		function (...) {
			x_( xFromChars...(self_(), ...) )
		}

	this$x_FromChars <-
		function () {
			xFromChars(self_())
		}
	this$x_FromChars... <-
		function (...) {
			xFromChars...(self_(), ...)
		}

	# --- xUnion --- #
	this$xUnion <-
		xMethod(xUnion, 'colls')

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
		xMethod(xUnit, 'coll')

	this$x_Unit <-
		function () {
			xUnit(self_())
		}

	# --- xUnique --- #
	this$xUnique <-
		xMethod(xUnique, 'coll')

	this$x_Unique <-
		function () {
			xUnique(self_())
		}

	this$xUnique... <-
		function (...) {
			x_( xUnique(self_(), ...)	)
		}

	this$x_Unique... <-
		function (...) {
			xUnique(self_(), ...)
		}

	# --- xFromLines --- #
	this$xFromLines <-
		xMethod(xFromLines, 'strs')

	this$xFromLines... <-
		function (...) {
			x_( xFromLines...(self_(), ...) )
		}

	this$x_FromLines <-
		function () {
			xFromLines(self_())
		}
	this$x_FromLines... <-
		function (...) {
			xFromLines...(self_(), ...)
		}

	# --- xFromWords --- #
	this$xFromWords <-
		xMethod(xFromWords, 'strs')

	this$xFromWords... <-
		function (...) {
			x_( xFromWords...(self_(), ...) )
		}

	this$x_FromWords <-
		function () {
			xFromWords(self_())
		}
	this$x_FromWords... <-
		function (...) {
			xFromWords...(self_(), ...)
		}

 	# -------- V ------- #
	# -------- W ------- #
	# --- xToWords --- #
	this$xToWords <-
		xMethod(xToWords, 'str')

	this$x_ToWords <-
		function () {
			xToWords(self_())
		}

	this$xDelay <-
		function (fn) {
			x_( xDelay(fn, self_()) )
		}

	this$x_Wait <-
		function (fn) {
			xDelay(fn, self_())
		}

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
	# --- xZip --- #
	this$xZip <-
		xMethod(xZip, 'colls')

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
		xMethod(xZipWith, 'colls')

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
		contents_are = "collections")

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
			xApply(self_(), coll)
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
		function (val, coll) {
			x_( xFoldl(self_(), val, coll) )
		}
	this$xFoldl... <-
		function (val, ...) {
			x_( xFoldl...(self_(), val, ...) )
		}

	this$x_Foldl <-
		function (val, coll) {
			xFoldl(self_(), val, coll)
		}
	this$x_Foldl... <-
		function (val, ...) {
			xFoldl...(self_(), val, ...)
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
		function (val, coll) {
			x_( xFoldr(self_(), val, coll) )
		}
	this$xFoldr... <-
		function (val, ...) {
			x_( xFoldr...(self_(), val, ...) )
		}

	this$x_Foldr <-
		function (val, coll) {
			xFoldr(self_(), val, coll)
		}
	this$x_Foldr... <-
		function (val, ...) {
			xFoldr...(self_(), val, ...)
		}

	# --- xFoldListl --- #
	this$xFoldListl <-
		function (val, coll) {
			x_( xFoldListl(self_(), val, coll) )
		}
	this$xFoldListl... <-
		function (val, ...) {
			x_( xFoldListl...(self_(), val, ...) )
		}

	this$x_FoldListl <-
		function (val, coll) {
			xFoldListl(self_(), val, coll)
		}
	this$x_FoldListl... <-
		function (val, ...) {
			xFoldListl...(self_(), val, ...)
		}

	this$xFoldList <-
		function (val, coll) {
			x_( xFoldList(self_(), val, coll) )
		}
	this$xFoldList... <-
		function (val, ...) {
			x_( xFoldList...(self_(), val, ...) )
		}

	this$x_FoldList <-
		function (val, coll) {
			xFoldList(self_(), val, coll)
		}
	this$x_FoldList... <-
		function (val, ...) {
			xFoldList...(self_(), val, ...)
		}

	# --- xFormalsOf --- #
	this$xFormalsOf <-
		function () {
			x_( xFormalsOf(self_()) )
		}

	this$x_FormalsOf <-
		function () {
			xFormalsOf(self_())
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
		function (val) {
			x_( xIterate(self_(), val ) )
		}

	this$x_Iterate <-
		function (val) {
			xIterate(self_(), val )
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

	# --- xParamsOf --- #
	this$xParamsOf <-
		function () {
			x_( xParamsOf(self_()) )
		}

	this$x_ParamsOf <-
		function () {
			xParamsOf(self_())
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
	# --- xDeepMap --- #
	this$xDeepMap <-
		function (coll) {
			x_( xDeepMap(self_(), coll) )
		}
	this$xDeepMap... <-
		function (...) {
			x_( xDeepMap...(self_(), ...) )
		}

	this$x_RecurMap <-
		function (coll) {
			xDeepMap(self_(), coll)
		}
	this$x_RecurMap... <-
		function (...) {
			xDeepMap...(self_(), ...)
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

	# --- xDelay --- #
	this$xDelay <-
		function (num) {
			x_( xDelay(self_(), num) )
		}

	this$x_Wait <-
		function (num) {
			xDelay(self_(), num)
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
		xMethod(xZip, 'colls')

	this$xZip... <-
		function (...) {
			x_( xZip...(self_(), ...) )
		}

	this$x_Zip <-
		x_Method(xZip, 'colls')

	this$x_Zip... <-
		function (...) {
			xZip...(self_(), ...)
		}

	this <- as.environment(
		c(as.list(this), as.list(x_any_proto)) )
	this$private <- list(
		contents_are = "functions")

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
#'    x_()$x_() is required.
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
	} else if (is.matrix( val )) {
		x_matrix_proto
	} else  if (is.vector( val ) || is.pairlist( val )){
		x_coll_proto
	} else if (is.data.frame( val )) {
		x_data_frame_proto
	} else  if (is.factor( val )) {
		x_factor_proto
	} else {
		x_any_proto
	}
}

#' @method $ arrow

'$.arrow' <- local({

	# some methods are expected to have bad names;
	# meet the user half way and mention the better name.
	autosuggested <- list(
		'xFilter' =
			'xSelect',
		'xFilter...' =
			'xSelect...',
		'x_Filter' =
			'x_Select',
		'x_Filter...' =
			'x_Select...',

		'xAsNumeric' =
			'xAsDouble',
		'xAsNumeric...' =
			'xAsDouble...',
		'x_AsNumeric' =
			'x_AsDouble',
		'x_AsNumeric...' =
			'x_AsDouble...'
	)

	suggest_similar_method <- function (val, method_name, contents_are, invoking_call) {
		# given an incorrect method name throw an error
		# suggesting a similar

		proto_ref <- get_proto_ref(val)
		method_name <- method_name

		candidate_methds <- setdiff(ls(proto_ref), 'private')
		distances <- adist(method_name, candidate_methds)

		similar <- if (method_name %in% names(autosuggested)) {
			autosuggested[[method_name]]
		} else if (min(distances) < nchar(method_name) / 2) {

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
		'$x_()' %+% single_newline)

	print(x$x_())
}
