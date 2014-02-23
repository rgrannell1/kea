

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
# upon invocation of an x_()$method, the Self function is updated to return the
# value contained in the x_() object. The Self function should be unbound unless it is called by an x_() function,
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

add_x_method <- function (env, fn, fixed) {
	# generate a method from an input function.
	# SIDE EFFECTFULLY UPDATES ENV, since environments are
	# pass by reference.

	fn_name <- paste0(as.symbol(match.call()$fn))
	fn_sym <- as.symbol(gsub('^x_', 'x', fn_name))

	is_unchaining <- grepl('^x_', fn_name)
	is_variadic <- grepl('[.]{3}$', fn_name)

	fn <- match.fun(fn_sym)

	if ( fixed %!in% names(formals(fn)) ) {
		write_error('not a parametre of ' %+% paste0(fn_sym))
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
									quote(Self())
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
									quote(Self())
								} else {
									as.symbol(param)
								}
							}) )) ))
			})

	} else if (is_variadic) {

		params <- Reduce(
			function (acc, param) {

				if (as.symbol(param) == fixed) {

					if (fixed == '...') {
						c( acc, quote(Self()), as.symbol('...') )
					} else {
						c( acc, quote(Self()) )
					}


				} else {
					c(acc, as.symbol(param))
				}
			},
			names(formals(fn)),
			list()
		)

		body(method) <- if (is_unchaining) {
			# x_Method...

			bquote({

				.(( as.call(c(fn_sym, params)) ))
			})

		} else {
			# x_Method...

			bquote({

				x_( .(( as.call(c(fn_sym, params)) )) )
			})
		}
	}

	# ESSENTIAL for closures; create a new
	# environment inheriting from the old functions environment.
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
			x_(Self())
		}

	this$x_Execute <-
		function (fn) {

			fn()
			Self()
		}

	# -------- F ------- #
	# -------- G ------- #
	this$xGraft <-
		function (str, fn) {
			# add a function to the x_
			# call chain for the
			# current R session.

			chainable <- function (...) {
				x_(fn(Self(), ...))
			}

			proto_ref <- get_proto_ref(Self())
			assign(str, chainable, envir = proto_ref)
		}

	# -------- H ------- #
	# -------- I ------- #
	add_x_method(this, xIdentity, 'val')
	add_x_method(this, x_Identity, 'val')

	# --- xIsEmpty --- #
	add_x_method(this, xIsEmpty, 'val')
	add_x_method(this, xIsEmpty..., '...')
	add_x_method(this, x_IsEmpty, 'val')
	add_x_method(this, x_IsEmpty..., '...')

	add_x_method(this, xIsNa, 'val')
	add_x_method(this, x_IsNa, 'val')

	add_x_method(this, xIsNan, 'val')
	add_x_method(this, x_IsNan, 'val')

	add_x_method(this, xIsNull, 'val')
	add_x_method(this, x_IsNull, 'val')

	add_x_method(this, xIsTrue, 'val')
	add_x_method(this, x_IsTrue, 'val')

	add_x_method(this, xIsFalse, 'val')
	add_x_method(this, x_IsFalse, 'val')

	# -------- J ------- #
	# -------- K ------- #
	add_x_method(this, xK, 'val')
	add_x_method(this, x_K, 'val')

	# -------- L ------- #
	# -------- M ------- #
	# -------- N ------- #
	add_x_method(this, xNotNa, 'val')
	add_x_method(this, x_NotNa, 'val')

	add_x_method(this, xNotNan, 'val')
	add_x_method(this, x_NotNan, 'val')

	add_x_method(this, xNotNull, 'val')
	add_x_method(this, x_NotNull, 'val')

	add_x_method(this, xNotTrue, 'val')
	add_x_method(this, x_NotTrue, 'val')

	add_x_method(this, xNotFalse, 'val')
	add_x_method(this, x_NotFalse, 'val')


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

			x_( fn(Self()) )
		}

	this$x_Tap <-
		function (fn) {

			fn(Self())
		}
	# -------- U ------- #
	# -------- V ------- #
	add_x_method(this, xVersion, '...')
	add_x_method(this, x_Version, '...')

	# -------- W ------- #
	# -------- X ------- #
	this$x_ <-
		function () {
			Self()
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
			dims <- dim(Self())

			if (dims[1] == 0 && dims[0] == 0) {
				x_( list() )
			} else if (dims[2] == 0) {
				x_( list() )
			} else if (dims[1] == 0) {
				x_( replicate(max(dims), list()))
			} else {
				x_( apply(Self(), 2, as.list) )
			}
		}

	this$x_ByCols <-
		function () {
			dims <- dim(Self())

			if (dims[1] == 0 && dims[0] == 0) {
				list()
			} else if (dims[2] == 0) {
				list()
			} else if (dims[1] == 0) {
				replicate(max(dims), list())
			} else {
				apply(Self(), 2, as.list)
			}
		}

	this$xByColnames <-
		function () {
			x_( as.list( colnames(Self()) ) )
		}
	this$x_ByColnames <-
		function () {
			as.list( colnames(Self()) )
		}

	this$xByRows <-
		function () {
			dims <- dim(Self())

			if (dims[1] == 0 && dims[0] == 0) {
				x_( list() )
			} else if (dims[1] == 0) {
				x_( list() )
			} else if (dims[2] == 0) {
				x_( replicate(max(dims), list()) )
			} else {
				x_( apply(Self(), 1, as.list) )
			}
		}

	this$x_ByRows <-
		function () {
			dims <- dim(Self())

			if (dims[1] == 0 && dims[0] == 0) {
				list()
			} else if (dims[1] == 0) {
				list()
			} else if (dims[2] == 0) {
				replicate(max(dims), list())
			} else {
				apply(Self(), 1, as.list)
			}
		}
	# --- xByRownames --- #
		this$xByRownames <-
			function () {
				x_( as.list( rownames(Self()) ) )
			}

		this$x_ByRownames <-
			function () {
				as.list( rownames(Self()) )
			}

	# -------- C ------- #
	this$xColUnit <-
		function () {
			x_(matrix(
				nrow = nrow(Self()),
				ncol = 0))
		}
	this$x_ColUnit <-
		function () {
			matrix(
				nrow = nrow(Self()),
				ncol = 0)
		}

	# -------- D ------- #
	# -------- E ------- #
	this$xElemsByCols <-
		function () {
			if (prod(dim(Self()) == 0)) {
				x_( list() )
			} else {
				x_( as.list(Self()) )
			}
		}
	this$x_ElemsByCols <-
		function () {
			if (prod(dim(Self()) == 0)) {
				list()
			} else {
				as.list(Self())
			}
		}


	this$xElemsByRows <-
		function () {
			if (prod(dim(Self()) == 0)) {
				x_( list() )
			} else {
				x_(as.list( t(Self()) ))
			}
		}
	this$x_ElemsByRows <-
		function () {
			if (prod(dim(Self()) == 0)) {
				list()
			} else {
				as.list( t(Self()) )
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
				ncol = ncol(Self()) ))
		}
	this$x_RowUnit <-
		function () {
			matrix(
				nrow = 0,
				ncol = ncol(Self()) )
		}

	# -------- S ------- #
	# -------- T ------- #
	this$xTranspose <-
		function () {
			x_( t(Self()) )
		}
	this$x_Transpose <-
		function () {
			t(Self())
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

	local({

		non_inherited <- ls(envir = this)
		inherited <- ls(envir = x_any_proto)

		non_overwritten <- setdiff(inherited, non_inherited)

		this <- as.environment(c(
			as.list(this),
			as.list(x_any_proto)[non_overwritten]) )

		this$private <- list(
			contents_are = "matrices")

		this

	})
})






x_data_frame_proto <- local({

	this <- Object()

	# -------- A ------- #

	# -------- B ------- #
	# --- xByCols --- #
	this$xByCols <-
		function () {
			x_(unname( as.list(Self()) ))
		}
	this$x_ByCols <-
		function () {
			unname( as.list(Self()) )
		}
	# --- xByColnames --- #
	this$xByColnames <-
		function () {
			x_( as.list( colnames(Self()) ) )
		}
	this$x_ByColnames <-
		function () {
			as.list( colnames(Self()) )
		}

	# --- xByRownames --- #
	this$xByRownames <-
		function () {
			x_( as.list( rownames(Self()) ) )
		}
	this$x_ByRownames <-
		function () {
			as.list( rownames(Self()) )
		}

	# -------- C ------- #
	# --- xColUnit --- #
	this$xColUnit <-
		function () {
			x_( unname(as.data.frame(
				matrix(
					nrow = nrow(Self()),
					ncol = 0)) ))
		}
	this$x_ColUnit <-
		function () {
			unname(as.data.frame(
				matrix(
					nrow = nrow(Self()),
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
					ncol = ncol(Self()) )) ))
		}
	this$x_RowUnit <-
		function () {
			unname(as.data.frame(
				matrix(
					nrow = 0,
					ncol = ncol(Self()) )) )
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

	local({

		non_inherited <- ls(envir = this)
		inherited <- ls(envir = x_any_proto)

		non_overwritten <- setdiff(inherited, non_inherited)

		this <- as.environment(c(
			as.list(this),
			as.list(x_any_proto)[non_overwritten]) )

		this$private <- list(
			contents_are = "data.frames")

		this

	})
})


x_factor_proto <- local({

	this <- Object()

	# -------- A ------- #
	# -------- B ------- #

	this$xByLevels <-
		function () {
			x_( as.character( levels(Self()) ) )
		}

	this$x_ByLevels <-
		function () {
			as.character( levels(Self()) )
		}

	this$xByValues <-
		function () {
			x_( as.vector(Self()) )
		}

	this$x_ByValues <-
		function () {
			as.vector(Self())
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

	local({

		non_inherited <- ls(envir = this)
		inherited <- ls(envir = x_any_proto)

		non_overwritten <- setdiff(inherited, non_inherited)

		this <- as.environment(c(
			as.list(this),
			as.list(x_any_proto)[non_overwritten]) )
		this$private <- list(
			contents_are = "factors")

		this

	})
})



# -------------------------------- Collection methods -------------------------------- #
#
# methods specific to vectors, lists or pairlists.
#
#
x_coll_proto <- local({

	this <- Object()

	# -------- A ------- #

	# --- xAt --- #
	add_x_method(this, xAt, 'coll')
	add_x_method(this, xAt..., '...')
	add_x_method(this, x_At, 'coll')
	add_x_method(this, x_At..., '...')

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
	add_x_method(this, xAsComplex, 'ims')
	add_x_method(this, xAsComplex..., '...')
	add_x_method(this, x_AsComplex, 'ims')
	add_x_method(this, x_AsComplex..., '...')

	# --- xAsDataFrame --- #
	add_x_method(this, xAsDataFrame, 'colls')
	add_x_method(this, xAsDataFrame..., '...')
	add_x_method(this, x_AsDataFrame, 'colls')
	add_x_method(this, x_AsDataFrame..., '...')

	# --- xApply --- #
	add_x_method(this, xApply, 'coll')
	add_x_method(this, xApply..., '...')
	add_x_method(this, x_Apply, 'coll')
	add_x_method(this, x_Apply..., '...')

	# --- xZipNames --- #
	add_x_method(this, xZipNames, 'colls')
	add_x_method(this, xZipNames..., '...')
	add_x_method(this, x_ZipNames, 'colls')
	add_x_method(this, x_ZipNames..., '...')

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

	# --- xCycle --- #
	add_x_method(this, xCycle, 'colls')
	add_x_method(this, xCycle..., '...')
	add_x_method(this, x_Cycle, 'colls')
	add_x_method(this, x_Cycle..., '...')

	# -------- D ------- #
	# --- xUnzipNames --- #
	add_x_method(this, xUnzipNames, 'coll')
	add_x_method(this, xUnzipNames..., '...')
	add_x_method(this, x_UnzipNames, 'coll')
	add_x_method(this, x_UnzipNames..., '...')

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

	# --- xDuplicatesOf --- #
	add_x_method(this, xDuplicatesOf, 'coll')
	add_x_method(this, xDuplicatesOf..., '...')
	add_x_method(this, x_DuplicatesOf, 'coll')
	add_x_method(this, x_DuplicatesOf..., '...')

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
	# --- xFirstOf --- #
	add_x_method(this, xFirstOf, 'coll')
	add_x_method(this, xFirstOf..., '...')
	add_x_method(this, x_FirstOf, 'coll')
	add_x_method(this, x_FirstOf..., '...')

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

	# --- xScanl --- #
	add_x_method(this, xScanl, 'coll')
	add_x_method(this, xScanl..., '...')
	add_x_method(this, x_Scanl, 'coll')
	add_x_method(this, x_Scanl..., '...')

	add_x_method(this, xScan, 'coll')
	add_x_method(this, xScan..., '...')
	add_x_method(this, x_Scan, 'coll')
	add_x_method(this, x_Scan..., '...')

	# --- xFourthOf --- #
	add_x_method(this, xFourthOf, 'coll')
	add_x_method(this, xFourthOf..., '...')
	add_x_method(this, x_FourthOf, 'coll')
	add_x_method(this, x_FourthOf..., '...')

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

	# --- xInitOf --- #
	add_x_method(this, xInitOf, 'coll')
	add_x_method(this, xInitOf..., '...')
	add_x_method(this, x_InitOf, 'coll')
	add_x_method(this, x_InitOf..., '...')

	# --- xElemIsFalse --- #
	add_x_method(this, xElemIsFalse, 'coll')
	add_x_method(this, xElemIsFalse..., '...')
	add_x_method(this, x_ElemIsFalse, 'coll')
	add_x_method(this, x_ElemIsFalse..., '...')

	# --- xElemIsTrue --- #
	add_x_method(this, xElemIsTrue, 'coll')
	add_x_method(this, xElemIsTrue..., '...')
	add_x_method(this, x_ElemIsTrue, 'coll')
	add_x_method(this, x_ElemIsTrue..., '...')

	# --- xElemIsNan --- #
	add_x_method(this, xElemIsNan, 'coll')
	add_x_method(this, xElemIsNan..., '...')
	add_x_method(this, x_ElemIsNan, 'coll')
	add_x_method(this, x_ElemIsNan..., '...')

	# --- xElemIsNa --- #
	add_x_method(this, xElemIsNa, 'coll')
	add_x_method(this, xElemIsNa..., '...')
	add_x_method(this, x_ElemIsNa, 'coll')
	add_x_method(this, x_ElemIsNa..., '...')

	# --- xElemIsNull --- #
	add_x_method(this, xElemIsNull, 'coll')
	add_x_method(this, xElemIsNull..., '...')
	add_x_method(this, x_ElemIsNull, 'coll')
	add_x_method(this, x_ElemIsNull..., '...')

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
	# --- xLastOf --- #
	add_x_method(this, xLastOf, 'coll')
	add_x_method(this, xLastOf..., '...')
	add_x_method(this, x_LastOf, 'coll')
	add_x_method(this, x_LastOf..., '...')

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

	# --- xLocate --- #

	add_x_method(this, xLocate, 'coll')
	add_x_method(this, xLocate..., '...')
	add_x_method(this, x_Locate, 'coll')
	add_x_method(this, x_Locate..., '...')

	# -------- M ------- #
	# --- xMap --- #
	add_x_method(this, xMap, 'coll')
	add_x_method(this, xMap..., '...')
	add_x_method(this, x_Map, 'coll')
	add_x_method(this, x_Map..., '...')

	# --- xMapply --- #
	add_x_method(this, xMapply, 'colls')
	add_x_method(this, xMapply..., '...')
	add_x_method(this, x_Mapply, 'colls')
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

	# --- xNotMember --- #
	add_x_method(this, xNotMember, 'coll')
	add_x_method(this, xNotMember..., '...')
	add_x_method(this, x_NotMember, 'coll')
	add_x_method(this, x_NotMember..., '...')

	# --- xNotEmpty --- #
	add_x_method(this, xNotEmpty, 'coll')
	add_x_method(this, xNotEmpty..., '...')
	add_x_method(this, x_NotEmpty, 'coll')
	add_x_method(this, x_NotEmpty..., '...')

	# --- xElemNotFalse --- #
	add_x_method(this, xElemNotFalse, 'coll')
	add_x_method(this, xElemNotFalse..., '...')
	add_x_method(this, x_ElemNotFalse, 'coll')
	add_x_method(this, x_ElemNotFalse..., '...')

	# --- xElemNotTrue --- #
	add_x_method(this, xElemNotTrue, 'coll')
	add_x_method(this, xElemNotTrue..., '...')
	add_x_method(this, x_ElemNotTrue, 'coll')
	add_x_method(this, x_ElemNotTrue..., '...')

	# --- xElemNotNa --- #
	add_x_method(this, xElemNotNa, 'coll')
	add_x_method(this, xElemNotNa..., '...')
	add_x_method(this, x_ElemNotNa, 'coll')
	add_x_method(this, x_ElemNotNa..., '...')

	# --- xElemNotNan --- #
	add_x_method(this, xElemNotNan, 'coll')
	add_x_method(this, xElemNotNan..., '...')
	add_x_method(this, x_ElemNotNan, 'coll')
	add_x_method(this, x_ElemNotNan..., '...')

	# --- xElemNotNull --- #
	add_x_method(this, xElemNotNull, 'coll')
	add_x_method(this, xElemNotNull..., '...')
	add_x_method(this, x_ElemNotNull, 'coll')
	add_x_method(this, x_ElemNotNull..., '...')

	# -------- O ------- #
	# --- xOneOf --- #
	add_x_method(this, xOneOf, 'coll')
	add_x_method(this, xOneOf..., '...')
	add_x_method(this, x_OneOf, 'coll')
	add_x_method(this, x_OneOf..., '...')

	# --- xOrder --- #
	add_x_method(this, xOrder, 'nums')
	add_x_method(this, xOrder..., '...')
	add_x_method(this, x_Order, 'nums')
	add_x_method(this, x_Order..., '...')

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
	add_x_method(this, xPartial..., '...')
	add_x_method(this, x_Partial, 'coll')
	add_x_method(this, x_Partial..., '...')

	# --- xPluck --- #
	add_x_method(this, xPluck, 'colls')
	add_x_method(this, xPluck..., '...')
	add_x_method(this, x_Pluck, 'colls')
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

	# --- xPredOf --- #
	add_x_method(this, xPredOf, 'nums')
	add_x_method(this, xPredOf..., '...')
	add_x_method(this, x_PredOf, 'nums')
	add_x_method(this, x_PredOf..., '...')

	# --- xPowerSetOf --- #
	add_x_method(this, xPowerSetOf, 'coll')
	add_x_method(this, xPowerSetOf..., '...')
	add_x_method(this, x_PowerSetOf, 'coll')
	add_x_method(this, x_PowerSetOf..., '...')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xRank --- #
	add_x_method(this, xRank, 'nums')
	add_x_method(this, xRank..., '...')
	add_x_method(this, x_Rank, 'nums')
	add_x_method(this, x_Rank..., '...')

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

	# --- xRestOf --- #
	add_x_method(this, xRestOf, 'coll')
	add_x_method(this, xRestOf..., '...')
	add_x_method(this, x_RestOf, 'coll')
	add_x_method(this, x_RestOf..., '...')

	# --- xReverse --- #
	add_x_method(this, xReverse, 'coll')
	add_x_method(this, xReverse..., '...')
	add_x_method(this, x_Reverse, 'coll')
	add_x_method(this, x_Reverse..., '...')

	# --- xRejectNa --- #
	add_x_method(this, xRejectNa, 'coll')
	add_x_method(this, xRejectNa..., '...')
	add_x_method(this, x_RejectNa, 'coll')
	add_x_method(this, x_RejectNa..., '...')

	# --- xRejectNan --- #
	add_x_method(this, xRejectNan, 'coll')
	add_x_method(this, xRejectNan..., '...')
	add_x_method(this, x_RejectNan, 'coll')
	add_x_method(this, x_RejectNan..., '...')

	# --- xRejectNull --- #
	add_x_method(this, xRejectNull, 'coll')
	add_x_method(this, xRejectNull..., '...')
	add_x_method(this, x_RejectNull, 'coll')
	add_x_method(this, x_RejectNull..., '...')

	# --- xRejectEmpty --- #
	add_x_method(this, xRejectEmpty, 'coll')
	add_x_method(this, xRejectEmpty..., '...')
	add_x_method(this, x_RejectEmpty, 'coll')
	add_x_method(this, x_RejectEmpty..., '...')

	# --- xReadLines --- #
	add_x_method(this, xReadLines, 'str')
	add_x_method(this, x_ReadLines, 'str')

	# --- xReadChars --- #
	add_x_method(this, xReadChars, 'str')
	add_x_method(this, x_ReadChars, 'str')

	# --- xReadWords --- #
	add_x_method(this, xReadWords, 'str')
	add_x_method(this, x_ReadWords, 'str')

	# -------- S ------- #
	# --- xSecondOf --- #
	add_x_method(this, xSecondOf, 'coll')
	add_x_method(this, xSecondOf..., '...')
	add_x_method(this, x_SecondOf, 'coll')
	add_x_method(this, x_SecondOf..., '...')

	# --- xProdSetOf --- #
	add_x_method(this, xProdSetOf, 'colls')
	add_x_method(this, xProdSetOf..., '...')
	add_x_method(this, x_ProdSetOf, 'colls')
	add_x_method(this, x_ProdSetOf..., '...')

	# --- xChunk --- #
	add_x_method(this, xChunk, 'coll')
	add_x_method(this, xChunk..., '...')
	add_x_method(this, x_Chunk, 'coll')
	add_x_method(this, x_Chunk..., '...')

	# --- xSelect --- #
	add_x_method(this, xSelect, 'coll')
	add_x_method(this, xSelect..., '...')
	add_x_method(this, x_Select, 'coll')
	add_x_method(this, x_Select..., '...')

	# --- xSigni --- #
	add_x_method(this, xSigni, 'nums')
	add_x_method(this, xSigni..., '...')
	add_x_method(this, x_Signi, 'nums')
	add_x_method(this, x_Signi..., '...')

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

	# --- xSplitBy --- #
	add_x_method(this, xSplitBy, 'coll')
	add_x_method(this, xSplitBy..., '...')
	add_x_method(this, x_SplitBy, 'coll')
	add_x_method(this, x_SplitBy..., '...')

	# --- xStopwatch --- #
	add_x_method(this, xStopwatch, 'num')
	add_x_method(this, x_Stopwatch, 'num')

	# --- x_Substring --- #
	add_x_method(this, xSubstring, 'str')
	add_x_method(this, x_Substring, 'str')

	# --- xSortBy --- #
	add_x_method(this, xSortBy, 'coll')
	add_x_method(this, xSortBy..., '...')
	add_x_method(this, x_SortBy, 'coll')
	add_x_method(this, x_SortBy..., '...')


	# --- xSuccOf --- #
	add_x_method(this, xSuccOf, 'nums')
	add_x_method(this, xSuccOf..., '...')
	add_x_method(this, x_SuccOf, 'nums')
	add_x_method(this, x_SuccOf..., '...')

	# -------- T ------- #
	# --- xTabulate --- #
	add_x_method(this, xTabulate, 'coll')
	add_x_method(this, xTabulate..., '...')
	add_x_method(this, x_Tabulate, 'coll')
	add_x_method(this, x_Tabulate..., '...')


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

	# --- xToWords --- #
	add_x_method(this, xToWords, 'str')
	add_x_method(this, x_ToWords, 'str')

	# --- xThirdOf --- #
	add_x_method(this, xThirdOf, 'coll')
	add_x_method(this, xThirdOf..., '...')
	add_x_method(this, x_ThirdOf, 'coll')
	add_x_method(this, x_ThirdOf..., '...')

	# -------- U ------- #
	# --- xUnion --- #
	add_x_method(this, xUnionOf, 'colls')
	add_x_method(this, xUnionOf..., '...')
	add_x_method(this, x_UnionOf, 'colls')
	add_x_method(this, x_UnionOf..., '...')

	# --- xUnit --- #
	add_x_method(this, xUnit, 'coll')
	add_x_method(this, x_Unit, 'coll')

	# --- xUniqueOf --- #
	add_x_method(this, xUniqueOf, 'coll')
	add_x_method(this, xUniqueOf..., '...')
	add_x_method(this, x_UniqueOf, 'coll')
	add_x_method(this, x_UniqueOf..., '...')

	# --- xFromLines --- #
	add_x_method(this, xFromLines, 'strs')
	add_x_method(this, xFromLines..., '...')
	add_x_method(this, x_FromLines, 'strs')
	add_x_method(this, x_FromLines..., '...')

	# --- xFromWords --- #
	add_x_method(this, xFromWords, 'strs')
	add_x_method(this, xFromWords..., '...')
	add_x_method(this, x_FromWords, 'strs')
	add_x_method(this, x_FromWords..., '...')

 	# -------- V ------- #
	# -------- W ------- #
	# --- xWriteChars --- #
	add_x_method(this, xWriteChars, 'str')
	add_x_method(this, x_WriteChars, 'str')

	# --- xWriteLines --- #
	add_x_method(this, xWriteLines, 'str')
	add_x_method(this, x_WriteLines, 'str')

	# --- xWriteWords --- #
	add_x_method(this, xWriteWords, 'str')
	add_x_method(this, x_WriteWords, 'str')

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

	local({

		non_inherited <- ls(envir = this)
		inherited <- ls(envir = x_any_proto)

		non_overwritten <- setdiff(inherited, non_inherited)

		this <- as.environment(c(
			as.list(this),
			as.list(x_any_proto)[non_overwritten]) )

		this$private <- list(
			contents_are = "collections")

		this

	})
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


	# --- xArityOf --- #
	add_x_method(this, xArityOf, 'fn')
	add_x_method(this, x_ArityOf, 'fn')

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
	add_x_method(this, x_Do, 'fn')
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

	# --- xScanl --- #
	add_x_method(this, xScanl, 'fn')
	add_x_method(this, xScanl..., 'fn')
	add_x_method(this, x_Scanl, 'fn')
	add_x_method(this, x_Scanl..., 'fn')

	add_x_method(this, xScan, 'fn')
	add_x_method(this, xScan..., 'fn')
	add_x_method(this, x_Scan, 'fn')
	add_x_method(this, x_Scan..., 'fn')

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

	# -------- L ------- #
	# --- xLimit --- #
	add_x_method(this, xLimit, 'fn')
	add_x_method(this, x_Limit, 'fn')

	# --- xLift --- #
	add_x_method(this, xLift, 'fn')
	add_x_method(this, xLift..., '...')
	add_x_method(this, x_Lift, 'fn')
	add_x_method(this, x_Lift..., '...')

	# --- xLocate --- #
	add_x_method(this, xLocate, 'pred')
	add_x_method(this, xLocate..., 'pred')
	add_x_method(this, x_Locate, 'pred')
	add_x_method(this, x_Locate..., 'pred')

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

	this$xReduce <- this$xReducel
	this$xReduce... <- this$xReducel...
	this$x_Reduce <- this$x_Reducel
	this$x_Reduce... <- this$x_Reducel...


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

	# --- xSortBy --- #
	add_x_method(this, xSortBy, 'pred')
	add_x_method(this, xSortBy..., '...')
	add_x_method(this, x_SortBy, 'pred')
	add_x_method(this, x_SortBy..., '...')

	# --- xSplitBy --- #
	add_x_method(this, xSplitBy, 'pred')
	add_x_method(this, xSplitBy..., 'pred')
	add_x_method(this, x_SplitBy, 'pred')
	add_x_method(this, x_SplitBy..., 'pred')

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

	local({

		non_inherited <- ls(envir = this)
		inherited <- ls(envir = x_any_proto)

		non_overwritten <- setdiff(inherited, non_inherited)

		this <- as.environment(c(
			as.list(this),
			as.list(x_any_proto)[non_overwritten]) )

		this$private <- list(
			contents_are = "functions")

		this

	})
})

# -------------------------------- Type Constructor -------------------------------- #

#' x_
#'
#' Generate an arrow object with methods available to it.
#'
#' @param
#'    val an arbitrary value. The value to wrap in an
#'    arrow object.
#'    The methods available depend on the input
#'    type; functions and collections have the most methods available.
#'
#' @return
#'    An object of class "arrow". Internally the object is represented as a
#'    list with a single field \bold{x}, but this field cannot be accessed directly.
#'    Instead, the method \bold{$ x_( )} or \bold{$ x_Identity( )} can be used to
#'    return the data stored in an arrow object.
#'
#'    The methods available to an arrow object depend on the type of the data it
#'    contains. All arrow objects inherit a handful of methods regardless of their
#'    type; these include \bold{xIdentity} and \bold{xTap} - a method that allows
#'    anonymous function to be executed on an arrow object.
#'
#'    Matrices, data frames, and factors have methods for converting them to collections,
#'    while normal Arrow functions are also available as methods for collections
#'    and functions.
#'
#' @section Corner Cases:
#'    The methods that can be used by \bold{$ x_( )} object varies depending
#'    on the type of val.
#'
#' @family method_functions
#'
#' @example
#'    inst/examples/example-x_.R
#'
#' @export

x_ <- function (val) {
	# Collection any -> Arrow any
	# type constructor for the method-chaining data type.

	invoking_call <- sys.call()

	insist $ must_not_be_missing(val)

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
	} else  if (is.atomic( val ) || is.list( val ) ||is.pairlist( val )){
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
#' @export

'$.arrow' <- local({

	# some methods are expected to have bad names;
	# meet the user half way and mention the better name.

	alias <- function (incorrect, correct) {

		forms <- function (fn_name) {

			list(
				fn_name,
				paste0(fn_name, '...'),
				gsub('^x', 'x_', fn_name),
				paste0(
					gsub('^x', 'x_', fn_name),
					 '...')
			)
		}

		structure(
			forms(correct),
			names = forms(incorrect))
	}

	autosuggested <- c(
		alias('x', 'x_'),
		alias('xAsNumeric', 'xAsDouble'),

		alias('xAsChars', 'xToChars'),
		alias('xAsWords', 'xToWords'),
		alias('xAsLines', 'xToLines'),

		alias('xToChars', 'xFromChars'),
		alias('xToWords', 'xFromWords'),
		alias('xToLines', 'xFromLines'),

		alias('xC', 'xJoin'),
		alias('xConcat', 'xJoin'),
		alias('xConcatenate', 'xJoin'),
		alias('xFilter', 'xSelect'),
		alias('xGroup', 'xChunk'),
		alias('xZipWith', 'xMapMany')
	)

	suggest_similar_method <- function (val, method_name, contents_are, invoking_call) {
		# given an incorrect method name throw an error
		# suggesting a similar

		proto_ref <- get_proto_ref(val)
		method_name <- method_name

		candidate_methods <- setdiff(ls(proto_ref), 'private')
		distances <- adist(method_name, candidate_methods)

		similar <- if (method_name %in% names(autosuggested)) {
			autosuggested[[method_name]]
		} else if (min(distances) < nchar(method_name) / 2) {

			candidate_methods[which.min(distances)]

		} else {
			character(0)
		}

		write_error(
			exclaim$method_not_found_(
				method_name, contents_are, similar),
			call. = False)
	}

	function (obj, method) {
		# Arrow a -> symbol -> function
		# return an arrow method associated with the type a.

		method_name <- paste0(match.call()$method)
		invoking_call <- paste0('$', method_name)

		proto_ref <- get_proto_ref( obj[['x']] )

		if (method_name %!in% ls(proto_ref) || method_name == "private") {
			# the invoked method wasn't found,
			# so we should give a suggestion.

			contents_are <- proto_ref[['private']][['contents_are']]

			suggest_similar_method(
				obj[['x']], method_name, contents_are, invoking_call)

		}

		fn <- proto_ref[[method_name]]
		environment(fn)[['Self']] <- function () obj[['x']]
		fn
	}
})

# -------------------------------- Print Method -------------------------------- #

#' @method print arrow
#' @export

print.arrow <- function (x, ...) {
	# custom print statement for the arrow object.

	proto_ref <- get_proto_ref( x[['x']] )
	contents_are <- proto_ref[['private']] [['contents_are']]

	single_newline <- '\n'
	double_newline <- '\n\n'

	header <- colourise$blue(
		'[ an arrow object with methods for ' %+% contents_are %+% ' ]')

	cat(
		header  %+% double_newline %+%
		'$x_()' %+% single_newline)

	print(x$x_())
}
