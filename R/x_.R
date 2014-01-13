

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

add_method <- function (env, fn, fixed) {
	# generate the xMethod form of the function.

	invoking_frame <- parent.frame()

	fn_sym <- as.symbol(match.call()$fn)
	fn_name <- paste0(fn_sym)

	is_unchaining <- grepl('^x_', fn_name)
	is_variadic <- grepl('[.]{3}$', fn_name)

	if (is_unchaining) {
		fn_sym <- as.symbol(gsub('^x_', 'x', fn_name))
	}

	if (length(fixed) > 0 && !(fixed %in% names(formals(fn)) )) {
		stop('not a parametre of ' %+% paste0(fn_sym))
	}

	method <- function () {

	}

	formals(method) <-
		formals(fn)[ names(formals(fn)) != fixed ]

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
		# xMethod

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
		# xMethod

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
		# xMethod

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

	environment(method) <- invoking_frame

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

	# -------- D ------- #

	# -------- E ------- #

		function (fn) {
			# execute a side-effectful function
			# before using the previous x_ monad
			# for further chaining.

			fn()
			x_(self_())
		}


		function (fn) {

			fn()
			self_()
		}
	# -------- F ------- #

	# -------- G ------- #

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

	add_method(xIdentity, 'val')
	add_method(x_Identity, 'val')


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

		function (fn) {
			# call an arbitrary function with self,
			# effectively allowing anonymous function
			# to execute arbitrary code before shunting
			# the output data back into the x_ monad.

			x_( fn(self_()) )
		}


		function (fn) {

			fn(self_())
		}
	# -------- U ------- #

	# -------- V ------- #

		xMethod(xVersion, character(0))


		xMethod(xVersion, character(0))

	# -------- W ------- #

	# -------- X ------- #

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


		function () {
			x_( as.list( colnames(self_()) ) )
		}

		function () {
			as.list( colnames(self_()) )
		}


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

			function () {
				x_( as.list( rownames(self_()) ) )
			}


			function () {
				as.list( rownames(self_()) )
			}

	# -------- C ------- #

		function () {
			x_(matrix(
				nrow = nrow(self_()),
				ncol = 0))
		}

		function () {
			matrix(
				nrow = nrow(self_()),
				ncol = 0)
		}

	# -------- D ------- #

	# -------- E ------- #

		function () {
			if (prod(dim(self_()) == 0)) {
				x_( list() )
			} else {
				x_( as.list(self_()) )
			}
		}

		function () {
			if (prod(dim(self_()) == 0)) {
				list()
			} else {
				as.list(self_())
			}
		}



		function () {
			if (prod(dim(self_()) == 0)) {
				x_( list() )
			} else {
				x_(as.list( t(self_()) ))
			}
		}

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

		function () {
			x_(matrix(
				nrow = 0,
				ncol = ncol(self_()) ))
		}

		function () {
			matrix(
				nrow = 0,
				ncol = ncol(self_()) )
		}

	# -------- S ------- #

	# -------- T ------- #

		function () {
			x_( t(self_()) )
		}

		function () {
			t(self_())
		}
	# -------- U ------- #

		function () {
			x_( matrix(nrow = 0, ncol = 0) )
		}

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

		function () {
			x_(unname( as.list(self_()) ))
		}

		function () {
			unname( as.list(self_()) )
		}
	# --- xByColnames --- #

		function () {
			x_( as.list( colnames(self_()) ) )
		}

		function () {
			as.list( colnames(self_()) )
		}

	# --- xByRownames --- #

		function () {
			x_( as.list( rownames(self_()) ) )
		}

		function () {
			as.list( rownames(self_()) )
		}

	# -------- C ------- #
	# --- xColUnit --- #

		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = nrow(self_()),
					ncol = 0)) ))
		}

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

		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = ncol(self_()) )) ))
		}

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

		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = 0 )) ))
		}

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


		function () {
			x_( as.character( levels(self_()) ) )
		}


		function () {
			as.character( levels(self_()) )
		}



		function () {
			x_( as.vector(self_()) )
		}


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
	add_method(this, xAsLogical, 'bools')
	add_method(this, xAsLogical..., '...')
	add_method(this, x_AsLogical, 'bools')
	add_method(this, x_AsLogical..., '...')

	# --- xAsInteger --- #
	add_method(this, xAsInteger, 'nums')
	add_method(this, xAsInteger..., '...')
	add_method(this, x_AsInteger, 'nums')
	add_method(this, x_AsInteger..., '...')

	# --- xAsCharacter --- #
	add_method(this, xAsCharacter, 'strs')
	add_method(this, xAsCharacter..., '...')
	add_method(this, xAsCharacter, 'strs')
	add_method(this, xAsCharacter..., '...')

	# --- xAsDouble --- #
	add_method(this, xAsDouble, 'nums')
	add_method(this, xAsDouble..., '...')
	add_method(this, xAsDouble, 'nums')
	add_method(this, xAsDouble..., '...')

	# --- xAsRaw --- #
	add_method(this, xAsRaw, 'raws')
	add_method(this, xAsRaw..., '...')
	add_method(this, xAsRaw, 'raws')
	add_method(this, xAsRaw..., '...')

	# --- xAsComplex --- #
	add_method(this, xAsComplex, 'comps')
	add_method(this, xAsComplex..., '...')
	add_method(this, xAsComplex, 'comps')
	add_method(this, xAsComplex, '...')

	# --- xAsFunction --- #
	add_method(this, xAsFunction, 'coll')
	add_method(this, xAsFunction..., '...')
	add_method(this, x_AsFunction, 'coll')
	add_method(this, x_AsFunction..., '...')

	# --- xApply --- #
	add_method(this, xApply, 'coll')
	add_method(this, xApply..., '...')
	add_method(this, x_Apply, 'coll')
	add_method(this, x_Apply..., '...')

	# --- xAssoc --- #
	add_method(this, xAssoc, 'colls')
	add_method(this, xAssoc, '...')
	add_method(this, x_Assoc, 'colls')
	add_method(this, x_Assoc..., '...')

	# -------- B ------- #
	# -------- C ------- #

	# --- xToChars --- #
	add_method(this, xToChars, 'str')
	add_method(this, x_ToChars, 'str')

	# --- xChop --- #
	add_method(this, xChop, 'coll')
	add_method(this, xChop..., '...')
	add_method(this, x_Chop, 'coll')
	add_method(this, x_Chop..., '...')

	# --- xCombos --- #
	add_method(this, xCombos, 'coll')
	add_method(this, xCombos..., '...')
	add_method(this, x_Combos, 'coll')
	add_method(this, x_Combos..., '...')

	# --- xConst --- #
	add_method(this, xConst, 'val')
	add_method(this, x_Const, 'val')

	# -------- D ------- #
	# --- xDissoc --- #
	add_method(this, xDissoc, 'colls')
	add_method(this, xDissoc..., '...')
	add_method(this, x_Dissoc, 'colls')
	add_method(this, x_Dissoc..., '...')

	# --- xDiffer --- #
	add_method(this, xDiffer, 'colls')
	add_method(this, xDiffer..., '...')
	add_method(this, x_Differ, 'colls')
	add_method(this, x_Differ..., '...')

	# --- xDrop --- #
	add_method(this, xDrop, 'coll')
	add_method(this, xDrop..., '...')
	add_method(this, x_Drop, 'coll')
	add_method(this, x_Drop..., '...')

	# --- xDo --- #
	add_method(this, xDo, 'coll')
	add_method(this, xDo..., '...')
	add_method(this, x_Do, 'coll')
	add_method(this, x_Do..., '...')

	# --- xDropWhile --- #
	add_method(xDropWhile, 'coll')
	add_method(xDropWhile..., '...')
	add_method(x_DropWhile, 'coll')
	add_method(x_DropWhile..., '...')

	# --- xDuplicated --- #
	add_method(xDuplicated, 'coll')
	add_method(xDuplicated..., '...')
	add_method(x_Duplicated, 'coll')
	add_method(x_Duplicated..., '...')

	# -------- E ------- #

	# --- xExists --- #
	add_method(xExists, 'colls')
	add_method(xExists..., '...')
	add_method(x_Exists, 'colls')
	add_method(x_Exists..., '...')

	# --- xExplode --- #
	add_method(xExplode, 'str')
	add_method(x_Explode, 'str')

	# -------- F ------- #

	# --- xFirst --- #
	add_method(xFirst, 'coll')
	add_method(xFirst..., '...')
	add_method(x_First, 'coll')
	add_method(x_First..., '...')

	# --- xFoldl --- #
	add_method(xFoldl, 'coll')
	add_method(xFoldl..., '...')
	add_method(x_Foldl, 'coll')
	add_method(x_Foldl..., '...')

	# --- xFold --- #
	this$xFold <- this$xFoldl
	this$xFold... <- this$xFoldl...

	this$x_Fold <- this$x_Foldl
	this$x_Fold... <- this$x_Foldl...

	# --- xFlatMap --- #
	add_method(xFlatMap, 'coll')
	add_method(xFlatMap..., '...')
	add_method(x_FlatMap, 'coll')
	add_method(x_FlatMap..., '...')

	# --- xFlatten --- #
	add_method(xFlatten, 'coll')
	add_method(xFlatten..., '...')
	add_method(x_Flatten, 'coll')
	add_method(x_Flatten..., '...')

	# --- xForall --- #
	add_method(xForall, 'colls')
	add_method(xForall..., '...')
	add_method(x_Forall, 'colls')
	add_method(x_Forall..., '...')

	# --- xFoldr --- #
	add_method(xFoldr, 'coll')
	add_method(xFoldr..., '...')
	add_method(x_Foldr, 'coll')
	add_method(x_Foldr..., '...')

	# --- xFoldListl --- #
	add_method(xFoldListl, 'coll')
	add_method(xFoldListl..., '...')
	add_method(xFoldListl, 'coll')
	add_method(xFoldListl..., '...')

	add_method(xFoldList, 'coll')
	add_method(xFoldList..., '...')
	add_method(xFoldList, 'coll')
	add_method(xFoldList..., '...')

	# --- xFourth --- #
	add_method(xFourth, 'coll')
	add_method(x_Fourth..., '...')
	add_method(xFourth, 'coll')
	add_method(x_Fourth..., '...')

	# -------- G ------- #
	add_method(xGetKey, 'str')
	add_method(x_GetKey, 'str')

	# -------- H ------- #
	# -------- I ------- #

	# --- xImplode --- #
	add_method(xImplode, 'strs')
	add_method(xImplode..., '...')
	add_method(x_Implode, 'strs')
	add_method(x_Implode..., '...')

	# --- xIsMember --- #
	add_method(xIsMember, 'coll')
	add_method(xIsMember..., '...')
	add_method(x_IsMember, 'coll')
	add_method(x_IsMember..., '...')

	# --- xInit --- #
	add_method(xInit, 'coll')
	add_method(xInit..., '...')
	add_method(x_Init, 'coll')
	add_method(x_Init..., '...')

	# --- xIsEmpty --- #
	add_method(xIsEmpty, 'coll')
	add_method(xIsEmpty..., '...')
	add_method(x_IsEmpty, 'coll')
	add_method(x_IsEmpty..., '...')

	# --- xIsFalse --- #
	add_method(xIsFalse, 'coll')
	add_method(xIsFalse..., '...')
	add_method(x_IsFalse, 'coll')
	add_method(x_IsFalse..., '...')

	# --- xIsTrue --- #
	add_method(xIsTrue, 'coll')
	add_method(xIsTrue..., '...')
	add_method(x_IsTrue, 'coll')
	add_method(x_IsTrue..., '...')

	# --- xIsNan --- #
	add_method(xIsNan, 'coll')
	add_method(xIsNan..., '...')
	add_method(x_IsNan, 'coll')
	add_method(x_IsNan..., '...')

	# --- xIsNa --- #
	add_method(xIsNa, 'coll')
	add_method(xIsNa..., '...')
	add_method(x_IsNa, 'coll')
	add_method(x_IsNa..., '...')

	# --- xIsNull --- #
	add_method(xIsNull, 'coll')
	add_method(xIsNull..., '...')
	add_method(x_IsNull, 'coll')
	add_method(x_IsNull..., '...')

	# --- xIterate --- #
	add_method(xIterate, 'val')
	add_method(x_Iterate, 'val')

	# --- xInter --- #
	add_method(xInter, 'colls')
	add_method(xInter..., '...')
	add_method(x_Inter, 'colls')
	add_method(x_Inter..., '...')

	# -------- J ------- #
	# --- xJoin --- #
	add_method(xJoin, 'colls')
	add_method(xJoin..., '...')
	add_method(x_Join, 'colls')
	add_method(x_Join..., '...')

	# --- xJuxtapose --- #
	add_method(xJuxtapose, 'fns')
	add_method(x_Juxtapose, 'fns')

	# -------- K ------- #
	# -------- L ------- #
	# --- xLast --- #
	add_method(xLast, 'coll')
	add_method(xLast..., '...')
	add_method(x_Last, 'coll')
	add_method(x_Last..., '...')

	# --- xLenOf --- #
	add_method(xLenOf, 'coll')
	add_method(xLenOf..., '...')
	add_method(x_LenOf, 'coll')
	add_method(x_LenOf..., '...')

	# --- xLimit --- #
	add_method(xLimit, 'num')
	add_method(x_Limit, 'num')

	# --- xToLines --- #
	add_method(xToLines, 'str')
	add_method(x_ToLines, 'str')

	# --- xLocatel --- #

	add_method(xLocatel, 'coll')
	add_method(xLocatel..., '...')
	add_method(x_Locatel, 'coll')
	add_method(x_Locatel..., '...')

	# --- xLocater --- #
	add_method(xLocater, 'coll')
	add_method(xLocater..., '...')
	add_method(x_Locater, 'coll')
	add_method(x_Locater..., '...')

	# -------- M ------- #
	# --- xMap --- #
	add_method(xMap, 'coll')
	add_method(xMap..., '...')
	add_method(x_Map, 'coll')
	add_method(x_Map..., '...')

	# --- xMapply --- #
	add_method(xMapply, 'coll')
	add_method(xMapply..., '...')
	add_method(x_Mapply, 'coll')
	add_method(x_Mapply..., '...')

	# --- xMapIndexed --- #
	add_method(xMapIndexed, 'coll')
	add_method(xMapIndexed..., '...')
	add_method(x_MapIndexed, 'coll')
	add_method(x_MapIndexed..., '...')

	# --- xMapMany --- #
	add_method(xMapMany, 'colls')
	add_method(xMapMany..., '...')
	add_method(x_MapMany, 'colls')
	add_method(x_MapMany..., '...')

	# -------- N ------- #
	# --- xAsNamed --- #
	add_method(xAsNamed, 'coll')
	add_method(x_AsNamed, 'coll')

	# --- xNegate --- #
	add_method(xNegate, 'nums')
	add_method(xNegate..., '...')
	add_method(x_Negate, 'nums')
	add_method(x_Negate..., '...')

	# --- xNotFalse --- #
	add_method(xNotFalse, 'coll')
	add_method(xNotFalse..., '...')
	add_method(x_NotFalse, 'coll')
	add_method(x_NotFalse..., '...')

	# --- xNotTrue --- #
	add_method(xNotTrue, 'coll')
	add_method(xNotTrue..., '...')
	add_method(x_NotTrue, 'coll')
	add_method(x_NotTrue..., '...')

	# --- xNotNa --- #
	add_method(xNotNa, 'coll')
	add_method(xNotNa..., '...')
	add_method(x_NotNa, 'coll')
	add_method(x_NotNa..., '...')

	# --- xNotNan --- #
	add_method(xNotNan, 'coll')
	add_method(xNotNan..., '...')
	add_method(x_NotNan, 'coll')
	add_method(x_NotNan..., '...')

	# -------- O ------- #
	# -------- P ------- #
	# --- xPack --- #
	add_method(xPack, 'coll')
	add_method(xPack..., '...')
	add_method(x_Pack, 'coll')
	add_method(x_Pack..., '...')

	# --- xPoll --- #
	add_method(xPoll, 'coll')
	add_method(xPoll..., '...')
	add_method(x_Poll, 'coll')
	add_method(x_Poll..., '...')

	# --- xPartial --- #
	add_method(xPartial, 'coll')
	add_method(x_Partial..., '...')
	add_method(xPartial, 'coll')
	add_method(x_Partial..., '...')

	# --- xPluck --- #
	add_method(xPluck, 'coll')
	add_method(xPluck..., '...')
	add_method(x_Pluck, 'coll')
	add_method(x_Pluck..., '...')

	# --- xPartition --- #
	add_method(xPartition, 'coll')
	add_method(xPartition, '...')
	add_method(x_Partition, 'coll')
	add_method(x_Partition, '...')

	# --- xPermute --- #
	add_method(xPermute, 'colls')
	add_method(xPermute..., '...')
	add_method(x_Permute, 'colls')
	add_method(x_Permute..., '...')

	# --- xPred --- #
	add_method(xPred, 'nums')
	add_method(xPred..., '...')
	add_method(x_Pred, 'nums')
	add_method(x_Pred..., '...')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xDeepMap --- #
	add_method(xDeepMap, 'coll')
	add_method(xDeepMap..., '...')
	add_method(x_DeepMap, 'coll')
	add_method(x_DeepMap..., '...')

	# --- xReducel --- #

	add_method(xReducel, 'coll')
	add_method(xReducel..., '...')
	add_method(x_Reducel, 'coll')
	add_method(x_Reducel..., '...')

	this$xReducel
	this$xReducel...


	this$x_Reducel
	this$x_Reducel...

	# --- xReducer --- #
	add_method(xReducer, 'coll')
	add_method(xReducer..., '...')
	add_method(x_Reducer, 'coll')
	add_method(x_Reducer..., '...')

	# --- xRepeat --- #
	add_method(xRepeat, 'coll')
	add_method(xRepeat..., '...')
	add_method(x_Repeat, 'coll')
	add_method(x_Repeat..., '...')

	# --- xReject --- #
	add_method(xReject, 'coll')
	add_method(xReject..., '...')
	add_method(x_Reject, 'coll')
	add_method(x_Reject..., '...')

	# --- xRest --- #
	add_method(xRest, 'coll')
	add_method(xRest..., '...')
	add_method(x_Rest, 'coll')
	add_method(x_Rest..., '...')

	# --- xReverse --- #
	add_method(xReverse, 'coll')
	add_method(xReverse..., '...')
	add_method(x_Reverse, 'coll')
	add_method(x_Reverse..., '...')

	# -------- S ------- #
	# --- xSecond --- #
	add_method(xSecond, 'coll')
	add_method(xSecond..., '...')
	add_method(x_Second, 'coll')
	add_method(x_Second..., '...')

	# --- xSetProd --- #
	add_method(xSetProd, 'colls')
	add_method(xSetProd..., '...')
	add_method(x_SetProd, 'colls')
	add_method(x_SetProd..., '...')

	# --- xGroup --- #
	add_method(xGroup, 'coll')
	add_method(xGroup..., '...')
	add_method(x_Group, 'coll')
	add_method(x_Group..., '...')

	# --- xSelect --- #
	add_method(xSelect, 'coll')
	add_method(xSelect..., '...')
	add_method(x_Select, 'coll')
	add_method(x_Select..., '...')

	# --- xSignum --- #
	add_method(xSignum, 'nums')
	add_method(xSignum..., '...')
	add_method(x_Signum, 'nums')
	add_method(x_Signum..., '...')

	# --- xSplitAt--- #
	add_method(xSplitAt, 'coll')
	add_method(xSplitAt..., '...')
	add_method(x_SplitAt, 'coll')
	add_method(x_SplitAt..., '...')

	# --- xShuffle --- #
	add_method(xShuffle, 'coll')
	add_method(xShuffle..., '...')
	add_method(x_Shuffle, 'coll')
	add_method(x_Shuffle..., '...')

	# --- xSplitWith --- #
	add_method(xSplitWith, 'coll')
	add_method(xSplitWith..., '...')
	add_method(xSplitWith, 'coll')
	add_method(xSplitWith..., '...')

	# --- xStopwatch --- #
	add_method(xStopwatch, 'num')
	add_method(x_Stopwatch, 'num')

	# --- xSubstring --- #
	add_method(xSubstring, 'str')
	add_method(x_Substring, 'str')

	# --- xSucc --- #
	add_method(xSucc, 'nums')
	add_method(xSucc..., '...')
	add_method(x_Succ, 'nums')
	add_method(x_Succ..., '...')

	# -------- T ------- #
	# --- xTake --- #
	add_method(xTake, 'coll')
	add_method(xTake..., '...')
	add_method(x_Take, 'coll')
	add_method(x_Take..., '...')

	# --- xTakeWhile --- #
	add_method(xTakeWhile, 'coll')
	add_method(xTakeWhile..., '...')
	add_method(x_TakeWhile, 'coll')
	add_method(x_TakeWhile..., '...')

	# --- xThird --- #
	add_method(xThird, 'coll')
	add_method(xThird..., '...')
	add_method(x_Third, 'coll')
	add_method(x_Third..., '...')

	# -------- U ------- #
	# --- xFromChars --- #
	add_method(xFromChars, 'strs')
	add_method(xFromChars..., '...')
	add_method(x_FromChars, 'strs')
	add_method(x_FromChars..., '...')

	# --- xUnion --- #
	add_method(xUnion, 'colls')
	add_method(xUnion..., '...')
	add_method(x_Union, 'colls')
	add_method(x_Union..., '...')

	# --- xUnit --- #
	add_method(xUnit, 'coll')
	add_method(x_Unit, 'coll')

	# --- xUnique --- #
	add_method(xUnique, 'coll')
	add_method(xUnique..., '...')
	add_method(x_Unique, 'coll')
	add_method(x_Unique..., '...')

	# --- xFromLines --- #
	add_method(xFromLines, 'strs')
	add_method(x_FromLines..., '...')
	add_method(xFromLines, 'strs')
	add_method(x_FromLines..., '...')

	# --- xFromWords --- #
	add_method(xFromWords, 'strs')
	add_method(xFromWords..., '...')
	add_method(x_FromWords, 'strs')
	add_method(x_FromWords..., '...')

 	# -------- V ------- #
	# -------- W ------- #
	# --- xToWords --- #
	add_method(xToWords, 'str')
	add_method(x_ToWords, 'str')

	# --- xDelay --- #
	add_method(xDelay, 'num')
	add_method(x_Delay, 'num')

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
	# --- xZip --- #
	add_method(xZip, 'colls')
	add_method(xZip..., '...')
	add_method(x_Zip, 'colls')
	add_method(x_Zip..., '...')

	# --- xZipWith --- #
	add_method(xZipWith, 'colls')
	add_method(xZipWith..., '...')
	add_method(x_ZipWith, 'colls')
	add_method(x_ZipWith..., '...')

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
	add_method(xAsClosure, 'fn')
	add_method(x_AsClosure, 'fn')

	# --- xAsUnary --- #
	add_method(xAsUnary, 'fn')
	add_method(x_AsUnary, 'fn')

	# --- xAsVariadic --- #
	add_method(xAsVariadic, 'fn')
	add_method(x_AsVariadic, 'fn')

	# --- xApply --- #
	add_method(xApply, 'fn')
	add_method(x_Apply, 'fn')

	# --- xArity --- #
	add_method(xArity, 'fn')
	add_method(x_Arity, 'fn')

	# -------- B ------- #


	# -------- C ------- #
	add_method(xCompose..., '...')
	add_method(x_Compose..., '...')
	# -------- D ------- #
	add_method(xDropWhile, 'pred')
	add_method(xDropWhile..., 'pred')
	add_method(x_DropWhile, 'pred')
	add_method(x_DropWhile..., 'pred')

	add_method(xDo, 'fn')
	add_method(xDo, 'fn')
	add_method(xDo..., 'fn')
	add_method(x_Do..., 'fn')

	# -------- E ------- #
	add_method(xExists, 'pred')
	add_method(xExists..., 'pred')

	add_method(x_Exists, 'pred')
	add_method(x_Exists..., 'pred')
	# -------- F ------- #
	add_method(xFlip, 'fn')
	add_method(x_Flip, 'fn')

	# --- xFlatMap --- #
	add_method(xFlatMap, 'fn')
	add_method(xFlatMap..., 'fn')
	add_method(x_FlatMap, 'fn')
	add_method(x_FlatMap..., 'fn')

	# --- xForall --- #
	add_method(xForall, 'pred')
	add_method(xForall..., 'pred')
	add_method(x_Forall, 'pred')
	add_method(x_Forall..., 'pred')

	# --- xFold --- #
	add_method(xFoldl, 'fn')
	add_method(xFoldl..., 'fn')
	add_method(x_Foldl, 'fn')
	add_method(x_Foldl..., 'fn')

	this$xFoldl
	this$xFoldl...


	this$x_Foldl
	this$x_Foldl...

	# --- xFoldr --- #
	add_method(xFoldr, 'fn')
	add_method(xFoldr..., 'fn')
	add_method(x_Foldr, 'fn')
	add_method(x_Foldr..., 'fn')

	# --- xFoldListl --- #
	add_method(xFoldListl, 'fn')
	add_method(xFoldListl..., 'fn')
	add_method(x_FoldListl, 'fn')
	add_method(x_FoldListl..., 'fn')

	add_method(xFoldList, 'fn')
	add_method(xFoldList..., 'fn')
	add_method(x_FoldList, 'fn')
	add_method(x_FoldList..., 'fn')

	# --- xFormalsOf --- #
	add_method(xFormalsOf, 'fn')
	add_method(x_FormalsOf, 'fn')

	# -------- G ------- #

	# -------- H ------- #
	# -------- I ------- #
	# --- xIsVariadic --- #
	add_method(xIsVariadic, 'fn')
	add_method(x_IsVariadic, 'fn')

	# --- xIterate --- #
	add_method(xIterate, 'fn')
	add_method(x_Iterate, 'fn')

	# -------- J ------- #
	# --- xJuxtapose --- #
	add_method(xJuxtapose..., '...')
	add_method(x_Juxtapose..., '...')

	# -------- K ------- #
	# --- xK --- #
	this$xConst
	this$x_Const

	# -------- L ------- #
	# --- xLimit --- #
	add_method(xLimit, 'num')
	add_method(x_Limit, 'num')

	# --- xLocate --- #
	add_method(xLift, 'fn')
	add_method(xLift..., '...')
	add_method(x_Lift, 'fn')
	add_method(x_Lift..., '...')

	# --- xLocatel --- #
	add_method(xLocatel, 'pred')
	add_method(xLocatel..., 'pred')
	add_method(x_Locatel, 'pred')
	add_method(x_Locatel..., 'pred')

	# --- xLocater --- #
	add_method(xLocater, 'pred')
	add_method(xLocater..., 'pred')
	add_method(x_Locater, 'pred')
	add_method(x_Locater..., 'pred')

	# -------- M ------- #
	# --- xMap --- #
	add_method(xMap, 'fn')
	add_method(xMap..., 'fn')
	add_method(x_Map, 'fn')
	add_method(x_Map..., 'fn')

	# --- xMapply --- #
	add_method(xMapply, 'fn')
	add_method(xMapply..., 'fn')
	add_method(x_Mapply, 'fn')
	add_method(x_Mapply..., 'fn')

	# --- xMapIndexed --- #
	add_method(xMapIndexed, 'fn')
	add_method(xMapIndexed..., 'fn')
	add_method(x_MapIndexed, 'fn')
	add_method(x_MapIndexed..., 'fn')

	# --- xMapMany --- #
	add_method(xMapMany, 'fn')
	add_method(xMapMany..., 'fn')
	add_method(x_MapMany, 'fn')
	add_method(x_MapMany..., 'fn')

	# -------- N ------- #
	add_method(xNot, 'pred')
	add_method(x_Not, 'pred')

	# -------- O ------- #
	# -------- P ------- #
	# --- xPartition --- #
	add_method(xPartition, 'pred')
	add_method(xPartition..., 'pred')
	add_method(x_Partition, 'pred')
	add_method(x_Partition..., 'pred')

	# --- xParamsOf --- #
	add_method(xParamsOf, 'fn')
	add_method(x_ParamsOf, 'fn')

	# --- xPartial --- #
	add_method(xPartial, 'fn')
	add_method(xPartial..., 'fn')
	add_method(x_Partial, 'fn')
	add_method(x_Partial..., 'fn')

	# --- xPoll --- #
	add_method(xPoll, 'pred')
	add_method(xPoll..., 'pred')
	add_method(x_Poll, 'pred')
	add_method(x_Poll..., 'pred')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xDeepMap --- #
	add_method(xDeepMap, 'fn')
	add_method(xDeepMap..., 'fn')
	add_method(x_DeepMap, 'fn')
	add_method(x_DeepMap..., 'fn')

	# --- xReducel --- #
	add_method(xReducel, 'fn')
	add_method(xReducel..., 'fn')
	add_method(x_Reducel, 'fn')
	add_method(x_Reducel..., 'fn')

	# --- xReducer --- #
	add_method(xReducer, 'fn')
	add_method(xReducer..., 'fn')
	add_method(x_Reducer, 'fn')
	add_method(x_Reducer..., 'fn')

	# --- xReject --- #
	add_method(xReject, 'pred')
	add_method(xReject..., 'pred')
	add_method(x_Reject, 'pred')
	add_method(x_Reject..., 'pred')

	# -------- S ------- #
	# --- xSelect --- #
	add_method(xSelect, 'pred')
	add_method(xSelect..., 'pred')
	add_method(x_Select, 'pred')
	add_method(x_Select..., 'pred')

	# --- xSplitWith --- #
	add_method(xSplitWith, 'pred')
	add_method(xSplitWith..., 'pred')
	add_method(x_SplitWith, 'pred')
	add_method(x_SplitWith..., 'pred')

	# -------- T ------- #

	# --- xTakeWhile --- #
	add_method(xTakeWhile, 'pred')
	add_method(xTakeWhile..., 'pred')
	add_method(x_TakeWhile, 'pred')
	add_method(x_TakeWhile..., 'pred')

	# --- xT --- #

	# -------- U ------- #
	# -------- V ------- #
	# --- xVectorise --- #
	add_method(xVectorise, 'fn')
	add_method(x_Vectorise, 'fn')

	add_method(xVectorize, 'fn')
	add_method(x_Vectorize, 'fn')

	# -------- W ------- #

	# --- xDelay --- #
	add_method(xDelay, 'fn')
	add_method(x_Delay, 'fn')

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #

	# --- xZipWith --- #
	add_method(xZipWith, 'fn')
	add_method(xZipWith..., 'fn')
	add_method(x_ZipWith, 'fn')
	add_method(x_ZipWith..., 'fn')

	# --- xZip --- #

	add_method(xZip, 'fn')
	add_method(xZip..., 'fn')
	add_method(x_Zip, 'fn')
	add_method(x_Zip..., 'fn')

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
