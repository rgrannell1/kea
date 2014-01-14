

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
# chaining variadic methods: xMethod . There are similar, but take variadic arguments.
# non-chaining methods: xMethods. These exit the monad.
# non-chaining methods: xMethods... . These are variadic, and exit the monad into the land of normal types.








# -------------------------------- Method Creators -------------------------------- #
#
# I don't like creating code dynamically, but originally I had to write these methods
# by hand. Many bugs are prevented by creating the methods dynamically in this case.
#















add_x_method <- function (env, fn, fixed) {
	# generate the xMethod form of the function.

	fn_sym <- as.symbol(match.call()$fn)
	fn_name <- paste0(fn_sym)

	is_unchaining <- grepl('^x_', fn_name)
	is_variadic <- grepl('[.]{3}$', fn_name)

	if (is_unchaining) {
		fn_sym <- as.symbol(gsub('^x_', 'x', fn_name))
		fn <- match.fun(fn_sym)
	}

	if (!(fixed %in% names(formals(fn)) )) {
		stop('not a parametre of ' %+% paste0(fn_sym))
	}

	method <- function () {	}

	formals(method) <- if (fixed == '...') {

		formals(fn)

	} else {

		formals(fn)[ names(formals(fn)) != fixed ]
	}

	if (!is_unchaining && !is_variadic) {
		# xMethod

		body(method) <-
			bquote({

				x_(.(
					( as.call(c(
						fn_sym,
						lapply(
							names(formals(fn)),
							function (param) {

								if (as.symbol(param) == fixed) {
									quote(self_())
								} else {
									as.symbol(param)
								}
							}) )) ) ))
			})

	} else if (is_unchaining && !is_variadic) {
		# x_Method

		body(method) <-
			bquote({

				.(
					( as.call(c(
						fn_sym,
						lapply(
							names(formals(fn)),
							function (param) {

								if (as.symbol(param) == fixed) {
									quote(self_())
								} else {
									as.symbol(param)
								}
							}) )) ))
			})

	} else if (!is_unchaining && is_variadic) {
		# xMethod...

		params <- Reduce(
			function (acc, param) {

				if (as.symbol(param) == fixed) {

					if (fixed == '...') {
						c( acc, quote(self_()), as.symbol('...') )
					} else {
						c( acc, quote(self_()) )
					}

				} else {
					c(acc, as.symbol(param))
				}
			},
			names(formals(fn)),
			list()
		)

		body(method) <-
			bquote({

				x_( .(( as.call(c(fn_sym, params)) )) )
			})


	} else if (is_unchaining && is_variadic) {
		# x_Method...

		params <- Reduce(
			function (acc, param) {

				if (as.symbol(param) == fixed) {

					if (fixed == '...') {
						c( acc, quote(self_()), as.symbol('...') )
					} else {
						c( acc, quote(self_()) )
					}

				} else {
					c(acc, as.symbol(param))
				}
			},
			names(formals(fn)),
			list()
		)

		body(method) <-
			bquote({

				.(( as.call(c(fn_sym, params)) ))
			})

	}

	# ESSENTIAL for closures;
	environment(method) <- new.env(parent = environment(fn))

	# side-effectful update.
	env[[fn_name]] <- method

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
	# --- xConst --- #
	add_x_method(this, xConst, 'val')
	add_x_method(this, x_Const, 'val')

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
	add_x_method(this, xIdentity, 'val')
	add_x_method(this, x_Identity, 'val')

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
	add_x_method(this, xVersion, '...')
	add_x_method(this, x_Version, '...')

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
	this$xFullUnit <-
		function () {
			x_( matrix(nrow = 0, ncol = 0) )
		}
	this$x_FullUnit <-
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
	# --- xFull --- #
	this$xFullUnit <-
		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = 0 )) ))
		}
	this$x_FullUnit <-
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



# NEW SYNTAX
#this <- add_xMethod(xAsLogical, 'bools')



x_coll_proto <- local({

	this <- Object()

	# -------- A ------- #

	# --- xAsLogical --- #
	add_x_method(this, xAsLogical, 'bools')
	add_x_method(this, xAsLogical..., '...')
	add_x_method(this, x_AsLogical, 'bools')
	add_x_method(this, x_AsLogical..., '...')

	# --- xAsInteger --- #
	add_x_method(this, xAsInteger, 'nums')
	add_x_method(this, xAsInteger..., '...')
	add_x_method(this, x_AsInteger, 'nums')
	add_x_method(this, x_AsInteger..., '...')

	# --- xAsCharacter --- #
	add_x_method(this, xAsCharacter, 'strs')
	add_x_method(this, xAsCharacter..., '...')
	add_x_method(this, x_AsCharacter, 'strs')
	add_x_method(this, x_AsCharacter..., '...')

	# --- xAsDouble --- #
	add_x_method(this, xAsDouble, 'nums')
	add_x_method(this, xAsDouble..., '...')
	add_x_method(this, x_AsDouble, 'nums')
	add_x_method(this, x_AsDouble..., '...')

	# --- xAsRaw --- #
	add_x_method(this, xAsRaw, 'raws')
	add_x_method(this, xAsRaw..., '...')
	add_x_method(this, x_AsRaw, 'raws')
	add_x_method(this, x_AsRaw..., '...')

	# --- xAsComplex --- #
	add_x_method(this, xAsComplex, 'comps')
	add_x_method(this, xAsComplex..., '...')
	add_x_method(this, x_AsComplex, 'comps')
	add_x_method(this, x_AsComplex..., '...')

	# --- xAsFunction --- #
	add_x_method(this, xAsFunction, 'coll')
	add_x_method(this, xAsFunction..., '...')
	add_x_method(this, x_AsFunction, 'coll')
	add_x_method(this, x_AsFunction..., '...')

	# --- xApply --- #
	add_x_method(this, xApply, 'coll')
	add_x_method(this, xApply..., '...')
	add_x_method(this, x_Apply, 'coll')
	add_x_method(this, x_Apply..., '...')

	# --- xAssoc --- #
	add_x_method(this, xAssoc, 'colls')
	add_x_method(this, xAssoc..., '...')
	add_x_method(this, x_Assoc, 'colls')
	add_x_method(this, x_Assoc..., '...')

	# -------- B ------- #
	# -------- C ------- #

	# --- xChop --- #
	add_x_method(this, xChop, 'coll')
	add_x_method(this, xChop..., '...')
	add_x_method(this, x_Chop, 'coll')
	add_x_method(this, x_Chop..., '...')

	# --- xCombos --- #
	add_x_method(this, xCombos, 'coll')
	add_x_method(this, xCombos..., '...')
	add_x_method(this, x_Combos, 'coll')
	add_x_method(this, x_Combos..., '...')

	# -------- D ------- #
	# --- xDissoc --- #
	add_x_method(this, xDissoc, 'colls')
	add_x_method(this, xDissoc..., '...')
	add_x_method(this, x_Dissoc, 'colls')
	add_x_method(this, x_Dissoc..., '...')

	# --- xDiffer --- #
	add_x_method(this, xDiffer, 'colls')
	add_x_method(this, xDiffer..., '...')
	add_x_method(this, x_Differ, 'colls')
	add_x_method(this, x_Differ..., '...')

	# --- xDrop --- #
	add_x_method(this, xDrop, 'coll')
	add_x_method(this, xDrop..., '...')
	add_x_method(this, x_Drop, 'coll')
	add_x_method(this, x_Drop..., '...')

	# --- xDo --- #
	add_x_method(this, xDo, 'coll')
	add_x_method(this, xDo..., '...')
	add_x_method(this, x_Do, 'coll')
	add_x_method(this, x_Do..., '...')

	# --- xDropWhile --- #
	add_x_method(this, xDropWhile, 'coll')
	add_x_method(this, xDropWhile..., '...')
	add_x_method(this, x_DropWhile, 'coll')
	add_x_method(this, x_DropWhile..., '...')

	# --- xDuplicated --- #
	add_x_method(this, xDuplicated, 'coll')
	add_x_method(this, xDuplicated..., '...')
	add_x_method(this, x_Duplicated, 'coll')
	add_x_method(this, x_Duplicated..., '...')

	# -------- E ------- #
	# --- xExists --- #
	add_x_method(this, xExists, 'colls')
	add_x_method(this, xExists..., '...')
	add_x_method(this, x_Exists, 'colls')
	add_x_method(this, x_Exists..., '...')

	# --- xExplode --- #
	add_x_method(this, xExplode, 'str')
	add_x_method(this, x_Explode, 'str')

	# -------- F ------- #
	# --- xFirst --- #
	add_x_method(this, xFirst, 'coll')
	add_x_method(this, xFirst..., '...')
	add_x_method(this, x_First, 'coll')
	add_x_method(this, x_First..., '...')

	# --- xFoldl --- #
	add_x_method(this, xFoldl, 'coll')
	add_x_method(this, xFoldl..., '...')
	add_x_method(this, x_Foldl, 'coll')
	add_x_method(this, x_Foldl..., '...')

	# --- xFold --- #
	this$xFold <- this$xFoldl
	this$xFold... <- this$xFoldl...

	this$x_Fold <- this$x_Foldl
	this$x_Fold... <- this$x_Foldl...

	# --- xFlatMap --- #
	add_x_method(this, xFlatMap, 'coll')
	add_x_method(this, xFlatMap..., '...')
	add_x_method(this, x_FlatMap, 'coll')
	add_x_method(this, x_FlatMap..., '...')

	# --- xFlatten --- #
	add_x_method(this, xFlatten, 'coll')
	add_x_method(this, xFlatten..., '...')
	add_x_method(this, x_Flatten, 'coll')
	add_x_method(this, x_Flatten..., '...')

	# --- xForall --- #
	add_x_method(this, xForall, 'colls')
	add_x_method(this, xForall..., '...')
	add_x_method(this, x_Forall, 'colls')
	add_x_method(this, x_Forall..., '...')

	# --- xFromChars --- #
	add_x_method(this, xFromChars, 'strs')
	add_x_method(this, xFromChars..., '...')
	add_x_method(this, x_FromChars, 'strs')
	add_x_method(this, x_FromChars..., '...')

	# --- xFoldr --- #
	add_x_method(this, xFoldr, 'coll')
	add_x_method(this, xFoldr..., '...')
	add_x_method(this, x_Foldr, 'coll')
	add_x_method(this, x_Foldr..., '...')

	# --- xFoldListl --- #
	add_x_method(this, xFoldListl, 'coll')
	add_x_method(this, xFoldListl..., '...')
	add_x_method(this, xFoldListl, 'coll')
	add_x_method(this, xFoldListl..., '...')

	add_x_method(this, xFoldList, 'coll')
	add_x_method(this, xFoldList..., '...')
	add_x_method(this, xFoldList, 'coll')
	add_x_method(this, xFoldList..., '...')

	# --- xFourth --- #
	add_x_method(this, xFourth, 'coll')
	add_x_method(this, x_Fourth..., '...')
	add_x_method(this, x_Fourth, 'coll')
	add_x_method(this, x_Fourth..., '...')

	# -------- G ------- #
	add_x_method(this, xGetKey, 'str')
	add_x_method(this, x_GetKey, 'str')

	# -------- H ------- #
	# -------- I ------- #

	# --- xImplode --- #
	add_x_method(this, xImplode, 'strs')
	add_x_method(this, xImplode..., '...')
	add_x_method(this, x_Implode, 'strs')
	add_x_method(this, x_Implode..., '...')

	# --- xIsMember --- #
	add_x_method(this, xIsMember, 'coll')
	add_x_method(this, xIsMember..., '...')
	add_x_method(this, x_IsMember, 'coll')
	add_x_method(this, x_IsMember..., '...')

	# --- xInit --- #
	add_x_method(this, xInit, 'coll')
	add_x_method(this, xInit..., '...')
	add_x_method(this, x_Init, 'coll')
	add_x_method(this, x_Init..., '...')

	# --- xIsEmpty --- #
	add_x_method(this, xIsEmpty, 'coll')
	add_x_method(this, xIsEmpty..., '...')
	add_x_method(this, x_IsEmpty, 'coll')
	add_x_method(this, x_IsEmpty..., '...')

	# --- xIsFalse --- #
	add_x_method(this, xIsFalse, 'coll')
	add_x_method(this, xIsFalse..., '...')
	add_x_method(this, x_IsFalse, 'coll')
	add_x_method(this, x_IsFalse..., '...')

	# --- xIsTrue --- #
	add_x_method(this, xIsTrue, 'coll')
	add_x_method(this, xIsTrue..., '...')
	add_x_method(this, x_IsTrue, 'coll')
	add_x_method(this, x_IsTrue..., '...')

	# --- xIsNan --- #
	add_x_method(this, xIsNan, 'coll')
	add_x_method(this, xIsNan..., '...')
	add_x_method(this, x_IsNan, 'coll')
	add_x_method(this, x_IsNan..., '...')

	# --- xIsNa --- #
	add_x_method(this, xIsNa, 'coll')
	add_x_method(this, xIsNa..., '...')
	add_x_method(this, x_IsNa, 'coll')
	add_x_method(this, x_IsNa..., '...')

	# --- xIsNull --- #
	add_x_method(this, xIsNull, 'coll')
	add_x_method(this, xIsNull..., '...')
	add_x_method(this, x_IsNull, 'coll')
	add_x_method(this, x_IsNull..., '...')

	# --- xIterate --- #
	add_x_method(this, xIterate, 'val')
	add_x_method(this, x_Iterate, 'val')

	# --- xInter --- #
	add_x_method(this, xInter, 'colls')
	add_x_method(this, xInter..., '...')
	add_x_method(this, x_Inter, 'colls')
	add_x_method(this, x_Inter..., '...')

	# -------- J ------- #
	# --- xJoin --- #
	add_x_method(this, xJoin, 'colls')
	add_x_method(this, xJoin..., '...')
	add_x_method(this, x_Join, 'colls')
	add_x_method(this, x_Join..., '...')

	# --- xJuxtapose --- #
	add_x_method(this, xJuxtapose, 'fns')
	add_x_method(this, x_Juxtapose, 'fns')

	# -------- K ------- #
	# -------- L ------- #
	# --- xLast --- #
	add_x_method(this, xLast, 'coll')
	add_x_method(this, xLast..., '...')
	add_x_method(this, x_Last, 'coll')
	add_x_method(this, x_Last..., '...')

	# --- xLenOf --- #
	add_x_method(this, xLenOf, 'coll')
	add_x_method(this, xLenOf..., '...')
	add_x_method(this, x_LenOf, 'coll')
	add_x_method(this, x_LenOf..., '...')

	# --- xLimit --- #
	add_x_method(this, xLimit, 'num')
	add_x_method(this, x_Limit, 'num')

	# --- xToLines --- #
	add_x_method(this, xToLines, 'str')
	add_x_method(this, x_ToLines, 'str')

	# --- xLocatel --- #

	add_x_method(this, xLocatel, 'coll')
	add_x_method(this, xLocatel..., '...')
	add_x_method(this, x_Locatel, 'coll')
	add_x_method(this, x_Locatel..., '...')

	# --- xLocater --- #
	add_x_method(this, xLocater, 'coll')
	add_x_method(this, xLocater..., '...')
	add_x_method(this, x_Locater, 'coll')
	add_x_method(this, x_Locater..., '...')

	# -------- M ------- #
	# --- xMap --- #
	add_x_method(this, xMap, 'coll')
	add_x_method(this, xMap..., '...')
	add_x_method(this, x_Map, 'coll')
	add_x_method(this, x_Map..., '...')

	# --- xMapply --- #
	add_x_method(this, xMapply, 'coll')
	add_x_method(this, xMapply..., '...')
	add_x_method(this, x_Mapply, 'coll')
	add_x_method(this, x_Mapply..., '...')

	# --- xMapIndexed --- #
	add_x_method(this, xMapIndexed, 'coll')
	add_x_method(this, xMapIndexed..., '...')
	add_x_method(this, x_MapIndexed, 'coll')
	add_x_method(this, x_MapIndexed..., '...')

	# --- xMapMany --- #
	add_x_method(this, xMapMany, 'colls')
	add_x_method(this, xMapMany..., '...')
	add_x_method(this, x_MapMany, 'colls')
	add_x_method(this, x_MapMany..., '...')

	# -------- N ------- #
	# --- xAsNamed --- #
	add_x_method(this, xAsNamed, 'coll')
	add_x_method(this, x_AsNamed, 'coll')

	# --- xNegate --- #
	add_x_method(this, xNegate, 'nums')
	add_x_method(this, xNegate..., '...')
	add_x_method(this, x_Negate, 'nums')
	add_x_method(this, x_Negate..., '...')

	# --- xNotFalse --- #
	add_x_method(this, xNotFalse, 'coll')
	add_x_method(this, xNotFalse..., '...')
	add_x_method(this, x_NotFalse, 'coll')
	add_x_method(this, x_NotFalse..., '...')

	# --- xNotTrue --- #
	add_x_method(this, xNotTrue, 'coll')
	add_x_method(this, xNotTrue..., '...')
	add_x_method(this, x_NotTrue, 'coll')
	add_x_method(this, x_NotTrue..., '...')

	# --- xNotNa --- #
	add_x_method(this, xNotNa, 'coll')
	add_x_method(this, xNotNa..., '...')
	add_x_method(this, x_NotNa, 'coll')
	add_x_method(this, x_NotNa..., '...')

	# --- xNotNan --- #
	add_x_method(this, xNotNan, 'coll')
	add_x_method(this, xNotNan..., '...')
	add_x_method(this, x_NotNan, 'coll')
	add_x_method(this, x_NotNan..., '...')

	# -------- O ------- #
	# -------- P ------- #
	# --- xPack --- #
	add_x_method(this, xPack, 'coll')
	add_x_method(this, xPack..., '...')
	add_x_method(this, x_Pack, 'coll')
	add_x_method(this, x_Pack..., '...')

	# --- xPoll --- #
	add_x_method(this, xPoll, 'coll')
	add_x_method(this, xPoll..., '...')
	add_x_method(this, x_Poll, 'coll')
	add_x_method(this, x_Poll..., '...')

	# --- xPartial --- #
	add_x_method(this, xPartial, 'coll')
	add_x_method(this, x_Partial..., '...')
	add_x_method(this, xPartial, 'coll')
	add_x_method(this, x_Partial..., '...')

	# --- xPluck --- #
	add_x_method(this, xPluck, 'coll')
	add_x_method(this, xPluck..., '...')
	add_x_method(this, x_Pluck, 'coll')
	add_x_method(this, x_Pluck..., '...')

	# --- xPartition --- #
	add_x_method(this, xPartition, 'coll')
	add_x_method(this, xPartition..., '...')
	add_x_method(this, x_Partition, 'coll')
	add_x_method(this, x_Partition..., '...')

	# --- xPermute --- #
	add_x_method(this, xPermute, 'colls')
	add_x_method(this, xPermute..., '...')
	add_x_method(this, x_Permute, 'colls')
	add_x_method(this, x_Permute..., '...')

	# --- xPred --- #
	add_x_method(this, xPred, 'nums')
	add_x_method(this, xPred..., '...')
	add_x_method(this, x_Pred, 'nums')
	add_x_method(this, x_Pred..., '...')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xDeepMap --- #
	add_x_method(this, xDeepMap, 'coll')
	add_x_method(this, xDeepMap..., '...')
	add_x_method(this, x_DeepMap, 'coll')
	add_x_method(this, x_DeepMap..., '...')

	# --- xReducel --- #

	add_x_method(this, xReducel, 'coll')
	add_x_method(this, xReducel..., '...')
	add_x_method(this, x_Reducel, 'coll')
	add_x_method(this, x_Reducel..., '...')

	this$xReduce <- this$xReducel
	this$xReduce... <- this$xReducel...

	this$x_Reduce <- this$x_Reducel
	this$x_Reduce... <- this$x_Reducel...

	# --- xReducer --- #
	add_x_method(this, xReducer, 'coll')
	add_x_method(this, xReducer..., '...')
	add_x_method(this, x_Reducer, 'coll')
	add_x_method(this, x_Reducer..., '...')

	# --- xRepeat --- #
	add_x_method(this, xRepeat, 'coll')
	add_x_method(this, xRepeat..., '...')
	add_x_method(this, x_Repeat, 'coll')
	add_x_method(this, x_Repeat..., '...')

	# --- xReject --- #
	add_x_method(this, xReject, 'coll')
	add_x_method(this, xReject..., '...')
	add_x_method(this, x_Reject, 'coll')
	add_x_method(this, x_Reject..., '...')

	# --- xRest --- #
	add_x_method(this, xRest, 'coll')
	add_x_method(this, xRest..., '...')
	add_x_method(this, x_Rest, 'coll')
	add_x_method(this, x_Rest..., '...')

	# --- xReverse --- #
	add_x_method(this, xReverse, 'coll')
	add_x_method(this, xReverse..., '...')
	add_x_method(this, x_Reverse, 'coll')
	add_x_method(this, x_Reverse..., '...')

	# -------- S ------- #
	# --- xSecond --- #
	add_x_method(this, xSecond, 'coll')
	add_x_method(this, xSecond..., '...')
	add_x_method(this, x_Second, 'coll')
	add_x_method(this, x_Second..., '...')

	# --- xSetProd --- #
	add_x_method(this, xSetProd, 'colls')
	add_x_method(this, xSetProd..., '...')
	add_x_method(this, x_SetProd, 'colls')
	add_x_method(this, x_SetProd..., '...')

	# --- xGroup --- #
	add_x_method(this, xGroup, 'coll')
	add_x_method(this, xGroup..., '...')
	add_x_method(this, x_Group, 'coll')
	add_x_method(this, x_Group..., '...')

	# --- xSelect --- #
	add_x_method(this, xSelect, 'coll')
	add_x_method(this, xSelect..., '...')
	add_x_method(this, x_Select, 'coll')
	add_x_method(this, x_Select..., '...')

	# --- xSignum --- #
	add_x_method(this, xSignum, 'nums')
	add_x_method(this, xSignum..., '...')
	add_x_method(this, x_Signum, 'nums')
	add_x_method(this, x_Signum..., '...')

	# --- xSplitAt--- #
	add_x_method(this, xSplitAt, 'coll')
	add_x_method(this, xSplitAt..., '...')
	add_x_method(this, x_SplitAt, 'coll')
	add_x_method(this, x_SplitAt..., '...')

	# --- xShuffle --- #
	add_x_method(this, xShuffle, 'coll')
	add_x_method(this, xShuffle..., '...')
	add_x_method(this, x_Shuffle, 'coll')
	add_x_method(this, x_Shuffle..., '...')

	# --- xSplitWith --- #
	add_x_method(this, xSplitWith, 'coll')
	add_x_method(this, xSplitWith..., '...')
	add_x_method(this, xSplitWith, 'coll')
	add_x_method(this, xSplitWith..., '...')

	# --- xStopwatch --- #
	add_x_method(this, xStopwatch, 'num')
	add_x_method(this, x_Stopwatch, 'num')

	# --- xSubstring --- #
	add_x_method(this, xSubstring, 'str')
	add_x_method(this, x_Substring, 'str')

	# --- xSucc --- #
	add_x_method(this, xSucc, 'nums')
	add_x_method(this, xSucc..., '...')
	add_x_method(this, x_Succ, 'nums')
	add_x_method(this, x_Succ..., '...')

	# -------- T ------- #
	# --- xTake --- #
	add_x_method(this, xTake, 'coll')
	add_x_method(this, xTake..., '...')
	add_x_method(this, x_Take, 'coll')
	add_x_method(this, x_Take..., '...')

	# --- xTakeWhile --- #
	add_x_method(this, xTakeWhile, 'coll')
	add_x_method(this, xTakeWhile..., '...')
	add_x_method(this, x_TakeWhile, 'coll')
	add_x_method(this, x_TakeWhile..., '...')

	# --- xToChars --- #
	add_x_method(this, xToChars, 'str')
	add_x_method(this, x_ToChars, 'str')

	# --- xThird --- #
	add_x_method(this, xThird, 'coll')
	add_x_method(this, xThird..., '...')
	add_x_method(this, x_Third, 'coll')
	add_x_method(this, x_Third..., '...')

	# -------- U ------- #
	# --- xUnion --- #
	add_x_method(this, xUnion, 'colls')
	add_x_method(this, xUnion..., '...')
	add_x_method(this, x_Union, 'colls')
	add_x_method(this, x_Union..., '...')

	# --- xUnit --- #
	add_x_method(this, xUnit, 'coll')
	add_x_method(this, x_Unit, 'coll')

	# --- xUnique --- #
	add_x_method(this, xUnique, 'coll')
	add_x_method(this, xUnique..., '...')
	add_x_method(this, x_Unique, 'coll')
	add_x_method(this, x_Unique..., '...')

	# --- xFromLines --- #
	add_x_method(this, xFromLines, 'strs')
	add_x_method(this, x_FromLines..., '...')
	add_x_method(this, xFromLines, 'strs')
	add_x_method(this, x_FromLines..., '...')

	# --- xFromWords --- #
	add_x_method(this, xFromWords, 'strs')
	add_x_method(this, xFromWords..., '...')
	add_x_method(this, x_FromWords, 'strs')
	add_x_method(this, x_FromWords..., '...')

 	# -------- V ------- #
	# -------- W ------- #
	# --- xToWords --- #
	add_x_method(this, xToWords, 'str')
	add_x_method(this, x_ToWords, 'str')

	# --- xDelay --- #
	add_x_method(this, xDelay, 'num')
	add_x_method(this, x_Delay, 'num')

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
	# --- xZip --- #
	add_x_method(this, xZip, 'colls')
	add_x_method(this, xZip..., '...')
	add_x_method(this, x_Zip, 'colls')
	add_x_method(this, x_Zip..., '...')

	# --- xZipWith --- #
	add_x_method(this, xZipWith, 'colls')
	add_x_method(this, xZipWith..., '...')
	add_x_method(this, x_ZipWith, 'colls')
	add_x_method(this, x_ZipWith..., '...')

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
	add_x_method(this, xAsClosure, 'fn')
	add_x_method(this, x_AsClosure, 'fn')

	# --- xAsUnary --- #
	add_x_method(this, xAsUnary, 'fn')
	add_x_method(this, x_AsUnary, 'fn')

	# --- xAsVariadic --- #
	add_x_method(this, xAsVariadic, 'fn')
	add_x_method(this, x_AsVariadic, 'fn')

	# --- xApply --- #
	add_x_method(this, xApply, 'fn')
	add_x_method(this, xApply..., 'fn')
	add_x_method(this, x_Apply, 'fn')
	add_x_method(this, x_Apply..., 'fn')


	# --- xArity --- #
	add_x_method(this, xArity, 'fn')
	add_x_method(this, x_Arity, 'fn')

	# -------- B ------- #
	# -------- C ------- #
	add_x_method(this, xCompose..., '...')
	add_x_method(this, x_Compose..., '...')
	# -------- D ------- #
	add_x_method(this, xDropWhile, 'pred')
	add_x_method(this, xDropWhile..., 'pred')
	add_x_method(this, x_DropWhile, 'pred')
	add_x_method(this, x_DropWhile..., 'pred')

	add_x_method(this, xDo, 'fn')
	add_x_method(this, xDo, 'fn')
	add_x_method(this, xDo..., 'fn')
	add_x_method(this, x_Do..., 'fn')

	# -------- E ------- #
	add_x_method(this, xExists, 'pred')
	add_x_method(this, xExists..., 'pred')

	add_x_method(this, x_Exists, 'pred')
	add_x_method(this, x_Exists..., 'pred')
	# -------- F ------- #
	add_x_method(this, xFlip, 'fn')
	add_x_method(this, x_Flip, 'fn')

	# --- xFlatMap --- #
	add_x_method(this, xFlatMap, 'fn')
	add_x_method(this, xFlatMap..., 'fn')
	add_x_method(this, x_FlatMap, 'fn')
	add_x_method(this, x_FlatMap..., 'fn')

	# --- xForall --- #
	add_x_method(this, xForall, 'pred')
	add_x_method(this, xForall..., 'pred')
	add_x_method(this, x_Forall, 'pred')
	add_x_method(this, x_Forall..., 'pred')

	# --- xFold --- #
	add_x_method(this, xFoldl, 'fn')
	add_x_method(this, xFoldl..., 'fn')
	add_x_method(this, x_Foldl, 'fn')
	add_x_method(this, x_Foldl..., 'fn')

	this$xFold <- this$xFoldl
	this$xFold... <- this$xFoldl...

	this$x_Fold <- this$x_Foldl
	this$x_Fold... <- this$x_Foldl...

	# --- xFoldr --- #
	add_x_method(this, xFoldr, 'fn')
	add_x_method(this, xFoldr..., 'fn')
	add_x_method(this, x_Foldr, 'fn')
	add_x_method(this, x_Foldr..., 'fn')

	# --- xFoldListl --- #
	add_x_method(this, xFoldListl, 'fn')
	add_x_method(this, xFoldListl..., 'fn')
	add_x_method(this, x_FoldListl, 'fn')
	add_x_method(this, x_FoldListl..., 'fn')

	add_x_method(this, xFoldList, 'fn')
	add_x_method(this, xFoldList..., 'fn')
	add_x_method(this, x_FoldList, 'fn')
	add_x_method(this, x_FoldList..., 'fn')

	# --- xFormalsOf --- #
	add_x_method(this, xFormalsOf, 'fn')
	add_x_method(this, x_FormalsOf, 'fn')

	# -------- G ------- #

	# -------- H ------- #
	# -------- I ------- #
	# --- xIsVariadic --- #
	add_x_method(this, xIsVariadic, 'fn')
	add_x_method(this, x_IsVariadic, 'fn')

	# --- xIterate --- #
	add_x_method(this, xIterate, 'fn')
	add_x_method(this, x_Iterate, 'fn')

	# -------- J ------- #
	# --- xJuxtapose --- #
	add_x_method(this, xJuxtapose..., '...')
	add_x_method(this, x_Juxtapose..., '...')

	# -------- K ------- #
	# --- xK --- #
	this$xK <- this$xConst
	this$x_K <- this$x_Const

	# -------- L ------- #
	# --- xLimit --- #
	add_x_method(this, xLimit, 'num')
	add_x_method(this, x_Limit, 'num')

	# --- xLocate --- #
	add_x_method(this, xLift, 'fn')
	add_x_method(this, xLift..., '...')
	add_x_method(this, x_Lift, 'fn')
	add_x_method(this, x_Lift..., '...')

	# --- xLocatel --- #
	add_x_method(this, xLocatel, 'pred')
	add_x_method(this, xLocatel..., 'pred')
	add_x_method(this, x_Locatel, 'pred')
	add_x_method(this, x_Locatel..., 'pred')

	# --- xLocater --- #
	add_x_method(this, xLocater, 'pred')
	add_x_method(this, xLocater..., 'pred')
	add_x_method(this, x_Locater, 'pred')
	add_x_method(this, x_Locater..., 'pred')

	# -------- M ------- #
	# --- xMap --- #
	add_x_method(this, xMap, 'fn')
	add_x_method(this, xMap..., 'fn')
	add_x_method(this, x_Map, 'fn')
	add_x_method(this, x_Map..., 'fn')

	# --- xMapply --- #
	add_x_method(this, xMapply, 'fn')
	add_x_method(this, xMapply..., 'fn')
	add_x_method(this, x_Mapply, 'fn')
	add_x_method(this, x_Mapply..., 'fn')

	# --- xMapIndexed --- #
	add_x_method(this, xMapIndexed, 'fn')
	add_x_method(this, xMapIndexed..., 'fn')
	add_x_method(this, x_MapIndexed, 'fn')
	add_x_method(this, x_MapIndexed..., 'fn')

	# --- xMapMany --- #
	add_x_method(this, xMapMany, 'fn')
	add_x_method(this, xMapMany..., 'fn')
	add_x_method(this, x_MapMany, 'fn')
	add_x_method(this, x_MapMany..., 'fn')

	# -------- N ------- #
	add_x_method(this, xNot, 'pred')
	add_x_method(this, x_Not, 'pred')

	# -------- O ------- #
	# -------- P ------- #
	# --- xPartition --- #
	add_x_method(this, xPartition, 'pred')
	add_x_method(this, xPartition..., 'pred')
	add_x_method(this, x_Partition, 'pred')
	add_x_method(this, x_Partition..., 'pred')

	# --- xParamsOf --- #
	add_x_method(this, xParamsOf, 'fn')
	add_x_method(this, x_ParamsOf, 'fn')

	# --- xPartial --- #
	add_x_method(this, xPartial, 'fn')
	add_x_method(this, xPartial..., 'fn')
	add_x_method(this, x_Partial, 'fn')
	add_x_method(this, x_Partial..., 'fn')

	# --- xPoll --- #
	add_x_method(this, xPoll, 'pred')
	add_x_method(this, xPoll..., 'pred')
	add_x_method(this, x_Poll, 'pred')
	add_x_method(this, x_Poll..., 'pred')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xDeepMap --- #
	add_x_method(this, xDeepMap, 'fn')
	add_x_method(this, xDeepMap..., 'fn')
	add_x_method(this, x_DeepMap, 'fn')
	add_x_method(this, x_DeepMap..., 'fn')

	# --- xReducel --- #
	add_x_method(this, xReducel, 'fn')
	add_x_method(this, xReducel..., 'fn')
	add_x_method(this, x_Reducel, 'fn')
	add_x_method(this, x_Reducel..., 'fn')

	# --- xReducer --- #
	add_x_method(this, xReducer, 'fn')
	add_x_method(this, xReducer..., 'fn')
	add_x_method(this, x_Reducer, 'fn')
	add_x_method(this, x_Reducer..., 'fn')

	# --- xReject --- #
	add_x_method(this, xReject, 'pred')
	add_x_method(this, xReject..., 'pred')
	add_x_method(this, x_Reject, 'pred')
	add_x_method(this, x_Reject..., 'pred')

	# -------- S ------- #
	# --- xSelect --- #
	add_x_method(this, xSelect, 'pred')
	add_x_method(this, xSelect..., 'pred')
	add_x_method(this, x_Select, 'pred')
	add_x_method(this, x_Select..., 'pred')

	# --- xSplitWith --- #
	add_x_method(this, xSplitWith, 'pred')
	add_x_method(this, xSplitWith..., 'pred')
	add_x_method(this, x_SplitWith, 'pred')
	add_x_method(this, x_SplitWith..., 'pred')

	# -------- T ------- #
	# --- xTakeWhile --- #
	add_x_method(this, xTakeWhile, 'pred')
	add_x_method(this, xTakeWhile..., 'pred')
	add_x_method(this, x_TakeWhile, 'pred')
	add_x_method(this, x_TakeWhile..., 'pred')

	# --- xT --- #

	# -------- U ------- #
	# -------- V ------- #
	# --- xVectorise --- #
	add_x_method(this, xVectorise, 'fn')
	add_x_method(this, x_Vectorise, 'fn')

	add_x_method(this, xVectorize, 'fn')
	add_x_method(this, x_Vectorize, 'fn')

	# -------- W ------- #

	# --- xDelay --- #
	add_x_method(this, xDelay, 'fn')
	add_x_method(this, x_Delay, 'fn')

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #

	# --- xZipWith --- #
	add_x_method(this, xZipWith, 'fn')
	add_x_method(this, xZipWith..., 'fn')
	add_x_method(this, x_ZipWith, 'fn')
	add_x_method(this, x_ZipWith..., 'fn')

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
