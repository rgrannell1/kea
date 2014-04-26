
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
# non-chaining methods: xMethods_ . These are variadic, and exit the monad into the land of normal types.








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
			# x_Method_

			bquote({

				.(( as.call(c(fn_sym, params)) ))
			})

		} else {
			# x_Method_

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
	# --- xCapture --- #
	add_x_method(this, xCapture, 'val')
	add_x_method(this, x_Capture, 'val')

	# -------- D ------- #
	# -------- E ------- #
	this$xExecute <-
		MakeFun(function (fn) {

			invoking_call <- sys.call()

			MACRO( Must $ Not_Be_Missing(fn) )

			MACRO( Must $ Be_Fn_Matchable(fn) )

			fn()
			x_(Self())
		})

	this$x_Execute <-
		MakeFun(function (fn) {

			invoking_call <- sys.call()

			MACRO( Must $ Not_Be_Missing(fn) )

			MACRO( Must $ Be_Fn_Matchable(fn) )

			fn()
			Self()
		})

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

			proto <- get_proto_ref(Self())
			assign(str, chainable, envir = proto_ref[[1]])
		}

	# -------- H ------- #
	# -------- I ------- #
	add_x_method(this, xIdentity, 'val')
	add_x_method(this, x_Identity, 'val')

	# --- xIsEmpty --- #
	add_x_method(this, xIsEmpty, 'coll')
	add_x_method(this, xIsEmpty_, '...')
	add_x_method(this, x_IsEmpty, 'coll')
	add_x_method(this, x_IsEmpty_, '...')

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
		MakeFun(function (fn) {

			invoking_call <- sys.call()

			MACRO( Must $ Not_Be_Missing(fn) )

			MACRO( Must $ Be_Fn_Matchable(fn) )

			x_( fn(Self()) )
		})

	this$x_Tap <-
		MakeFun(function (fn) {

			invoking_call <- sys.call()

			MACRO( Must $ Not_Be_Missing(fn) )

			MACRO( Must $ Be_Fn_Matchable(fn) )

			fn(Self())
		})

	this$xThread <-
		MakeFun(function (fns) {
			xThread(Self(), fns)
		})
	this$xThread_ <-
		MakeFun(function (...) {
			xThread_(Self(), ...)
		})
	this$x_Thread <-
		MakeFun(function (fns) {
			xThread(Self(), fns)
		})
	this$x_Thread_ <-
		MakeFun(function (...) {
			xThread_(Self(), ...)
		})
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

	this$xByColkeys <-
		function () {
			x_( as.list( colnames(Self()) ) )
		}
	this$x_ByColkeys <-
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
	# --- xByRowkeys --- #
		this$xByRowkeys <-
			function () {
				x_( as.list( rownames(Self()) ) )
			}

		this$x_ByRowkeys <-
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

	# --- xByColkeys --- #
	this$xByColkeys <-
		function () {
			x_( as.list( colnames(Self()) ) )
		}
	this$x_ByColkeys <-
		function () {
			as.list( colnames(Self()) )
		}

	# --- xByRowkeys --- #
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

	this$xByRowkeys <-
		function () {
			x_( as.list( rownames(Self()) ) )
		}
	this$x_ByRowkeys <-
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
	# --- xAny --- #
	add_x_method(this, xAny, 'coll')
	add_x_method(this, x_Any, 'coll')
	add_x_method(this, xAny_, '...')
	add_x_method(this, x_Any_, '...')

	# --- xAll --- #
	add_x_method(this, xAll, 'coll')
	add_x_method(this, x_All, 'coll')
	add_x_method(this, xAll_, '...')
	add_x_method(this, x_All_, '...')

	# --- xAt --- #
	add_x_method(this, xAt, 'coll')
	add_x_method(this, xAt_, '...')
	add_x_method(this, x_At, 'coll')
	add_x_method(this, x_At_, '...')

	# --- xAtKey --- #
	add_x_method(this, xAtKey, 'coll')
	add_x_method(this, xAtKey_, '...')
	add_x_method(this, x_AtKey, 'coll')
	add_x_method(this, x_AtKey_, '...')

	add_x_method(this, xAtCol, 'colls')
	add_x_method(this, xAtCol_, '...')
	add_x_method(this, x_AtCol, 'colls')
	add_x_method(this, x_AtCol_, '...')

	# --- xSlice --- #
	add_x_method(this, xSlice, 'coll')
	add_x_method(this, xSlice_, '...')
	add_x_method(this, x_Slice, 'coll')
	add_x_method(this, x_Slice_, '...')

	# --- xAsLogical --- #
	add_x_method(this, xAsLogical, 'bools')
	add_x_method(this, xAsLogical_, '...')
	add_x_method(this, x_AsLogical, 'bools')
	add_x_method(this, x_AsLogical_, '...')

	# --- xAsInteger --- #
	add_x_method(this, xAsInteger, 'nums')
	add_x_method(this, xAsInteger_, '...')
	add_x_method(this, x_AsInteger, 'nums')
	add_x_method(this, x_AsInteger_, '...')

	# --- xAsCharacter --- #
	add_x_method(this, xAsCharacter, 'strs')
	add_x_method(this, xAsCharacter_, '...')
	add_x_method(this, x_AsCharacter, 'strs')
	add_x_method(this, x_AsCharacter_, '...')

	# --- xAsDouble --- #
	add_x_method(this, xAsDouble, 'nums')
	add_x_method(this, xAsDouble_, '...')
	add_x_method(this, x_AsDouble, 'nums')
	add_x_method(this, x_AsDouble_, '...')

	# --- xAsRaw --- #
	add_x_method(this, xAsRaw, 'raws')
	add_x_method(this, xAsRaw_, '...')
	add_x_method(this, x_AsRaw, 'raws')
	add_x_method(this, x_AsRaw_, '...')

	# --- xAsComplex --- #
	add_x_method(this, xAsComplex, 'ims')
	add_x_method(this, xAsComplex_, '...')
	add_x_method(this, x_AsComplex, 'ims')
	add_x_method(this, x_AsComplex_, '...')

	# --- xAsDataFrame --- #
	add_x_method(this, xAsDataFrame, 'colls')
	add_x_method(this, xAsDataFrame_, '...')
	add_x_method(this, x_AsDataFrame, 'colls')
	add_x_method(this, x_AsDataFrame_, '...')

	# --- xApply --- #
	add_x_method(this, xApply, 'coll')
	add_x_method(this, xApply_, '...')
	add_x_method(this, x_Apply, 'coll')
	add_x_method(this, x_Apply_, '...')

	# --- xZipKeys --- #
	add_x_method(this, xZipKeys, 'colls')
	add_x_method(this, xZipKeys_, '...')
	add_x_method(this, x_ZipKeys, 'colls')
	add_x_method(this, x_ZipKeys_, '...')

	# -------- B ------- #
	# -------- C ------- #

	# --- xChop --- #
	add_x_method(this, xChop, 'coll')
	add_x_method(this, xChop_, '...')
	add_x_method(this, x_Chop, 'coll')
	add_x_method(this, x_Chop_, '...')

	# --- xChoose --- #
	add_x_method(this, xChoose, 'coll')
	add_x_method(this, xChoose_, '...')
	add_x_method(this, x_Choose, 'coll')
	add_x_method(this, x_Choose_, '...')

	# --- xCycle --- #
	add_x_method(this, xCycle, 'colls')
	add_x_method(this, xCycle_, '...')
	add_x_method(this, x_Cycle, 'colls')
	add_x_method(this, x_Cycle_, '...')

	# -------- D ------- #

	# --- xDrop --- #
	add_x_method(this, xDrop, 'coll')
	add_x_method(this, xDrop_, '...')
	add_x_method(this, x_Drop, 'coll')
	add_x_method(this, x_Drop_, '...')

	# --- xDo --- #
	add_x_method(this, xDo, 'coll')
	add_x_method(this, xDo_, '...')
	add_x_method(this, x_Do, 'coll')
	add_x_method(this, x_Do_, '...')

	# --- xDropWhile --- #
	add_x_method(this, xDropWhile, 'coll')
	add_x_method(this, xDropWhile_, '...')
	add_x_method(this, x_DropWhile, 'coll')
	add_x_method(this, x_DropWhile_, '...')

	# --- xDuplicatesOf --- #
	add_x_method(this, xDuplicatesOf, 'coll')
	add_x_method(this, xDuplicatesOf_, '...')
	add_x_method(this, x_DuplicatesOf, 'coll')
	add_x_method(this, x_DuplicatesOf_, '...')

	# -------- E ------- #
	# --- xExists --- #
	add_x_method(this, xExists, 'colls')
	add_x_method(this, xExists_, '...')
	add_x_method(this, x_Exists, 'colls')
	add_x_method(this, x_Exists_, '...')

	# --- xExplode --- #
	add_x_method(this, xExplode, 'str')

	# -------- F ------- #
	# --- xFirstOf --- #
	add_x_method(this, xFirstOf, 'coll')
	add_x_method(this, xFirstOf_, '...')
	add_x_method(this, x_FirstOf, 'coll')
	add_x_method(this, x_FirstOf_, '...')

	# --- xFold --- #
	add_x_method(this, xFold, 'coll')
	add_x_method(this, xFold_, '...')
	add_x_method(this, x_Fold, 'coll')
	add_x_method(this, x_Fold_, '...')

	# --- xFlatMap --- #
	add_x_method(this, xFlatMap, 'coll')
	add_x_method(this, xFlatMap_, '...')
	add_x_method(this, x_FlatMap, 'coll')
	add_x_method(this, x_FlatMap_, '...')

	# --- xFlatten --- #
	add_x_method(this, xFlatten, 'coll')
	add_x_method(this, xFlatten_, '...')
	add_x_method(this, x_Flatten, 'coll')
	add_x_method(this, x_Flatten_, '...')

	# --- xForall --- #
	add_x_method(this, xForall, 'colls')
	add_x_method(this, xForall_, '...')
	add_x_method(this, x_Forall, 'colls')
	add_x_method(this, x_Forall_, '...')

	# --- xFromChars --- #
	add_x_method(this, xFromChars, 'strs')
	add_x_method(this, xFromChars_, '...')
	add_x_method(this, x_FromChars, 'strs')
	add_x_method(this, x_FromChars_, '...')

	# --- xScan --- #
	add_x_method(this, xScan, 'coll')
	add_x_method(this, xScan_, '...')
	add_x_method(this, x_Scan, 'coll')
	add_x_method(this, x_Scan_, '...')

	# --- xFourthOf --- #
	add_x_method(this, xFourthOf, 'coll')
	add_x_method(this, xFourthOf_, '...')
	add_x_method(this, x_FourthOf, 'coll')
	add_x_method(this, x_FourthOf_, '...')

	# -------- G ------- #
	add_x_method(this, xGroupBy, 'coll')
	add_x_method(this, xGroupBy_, '...')
	add_x_method(this, x_GroupBy, 'coll')
	add_x_method(this, x_GroupBy_, '...')
	# -------- H ------- #
	# -------- I ------- #

	# --- xImplode --- #
	add_x_method(this, xImplode, 'strs')
	add_x_method(this, xImplode_, '...')
	add_x_method(this, x_Implode, 'strs')
	add_x_method(this, x_Implode_, '...')

	# --- xIsMember --- #
	add_x_method(this, xIsMember, 'coll')
	add_x_method(this, xIsMember_, '...')
	add_x_method(this, x_IsMember, 'coll')
	add_x_method(this, x_IsMember_, '...')

	# --- xInitOf --- #
	add_x_method(this, xInitOf, 'coll')
	add_x_method(this, xInitOf_, '...')
	add_x_method(this, x_InitOf, 'coll')
	add_x_method(this, x_InitOf_, '...')

	# --- xElemIsFalse --- #
	add_x_method(this, xElemIsFalse, 'coll')
	add_x_method(this, xElemIsFalse_, '...')
	add_x_method(this, x_ElemIsFalse, 'coll')
	add_x_method(this, x_ElemIsFalse_, '...')

	# --- xElemIsTrue --- #
	add_x_method(this, xElemIsTrue, 'coll')
	add_x_method(this, xElemIsTrue_, '...')
	add_x_method(this, x_ElemIsTrue, 'coll')
	add_x_method(this, x_ElemIsTrue_, '...')

	# --- xElemIsNan --- #
	add_x_method(this, xElemIsNan, 'coll')
	add_x_method(this, xElemIsNan_, '...')
	add_x_method(this, x_ElemIsNan, 'coll')
	add_x_method(this, x_ElemIsNan_, '...')

	# --- xElemIsNa --- #
	add_x_method(this, xElemIsNa, 'coll')
	add_x_method(this, xElemIsNa_, '...')
	add_x_method(this, x_ElemIsNa, 'coll')
	add_x_method(this, x_ElemIsNa_, '...')

	# --- xElemIsNull --- #
	add_x_method(this, xElemIsNull, 'coll')
	add_x_method(this, xElemIsNull_, '...')
	add_x_method(this, x_ElemIsNull, 'coll')
	add_x_method(this, x_ElemIsNull_, '...')

	# --- xIterate --- #
	add_x_method(this, xIterate, 'val')
	add_x_method(this, x_Iterate, 'val')

	# --- xInter --- #
	add_x_method(this, xInter, 'colls')
	add_x_method(this, xInter_, '...')
	add_x_method(this, x_Inter, 'colls')
	add_x_method(this, x_Inter_, '...')

	# --- xIsSubset --- #
	add_x_method(this, xIsSubset, 'coll2')
	add_x_method(this, xIsSubset_, '...')
	add_x_method(this, x_IsSubset, 'coll2')
	add_x_method(this, x_IsSubset_, '...')

	# -------- J ------- #
	# --- xJoin --- #
	add_x_method(this, xJoin, 'colls')
	add_x_method(this, xJoin_, '...')
	add_x_method(this, x_Join, 'colls')
	add_x_method(this, x_Join_, '...')

	# --- xJuxtapose --- #
	add_x_method(this, xJuxtapose, 'fns')
	add_x_method(this, x_Juxtapose, 'fns')

	# -------- K ------- #
	# -------- L ------- #
	# --- xLastOf --- #
	add_x_method(this, xLastOf, 'coll')
	add_x_method(this, xLastOf_, '...')
	add_x_method(this, x_LastOf, 'coll')
	add_x_method(this, x_LastOf_, '...')

	# --- xLenOf --- #
	add_x_method(this, xLenOf, 'coll')
	add_x_method(this, xLenOf_, '...')
	add_x_method(this, x_LenOf, 'coll')
	add_x_method(this, x_LenOf_, '...')

	# --- xLimit --- #
	add_x_method(this, xLimit, 'num')
	add_x_method(this, x_Limit, 'num')

	# --- xToLines --- #
	add_x_method(this, xToLines, 'str')
	add_x_method(this, x_ToLines, 'str')

	# --- xLocate --- #

	add_x_method(this, xLocate, 'coll')
	add_x_method(this, xLocate_, '...')
	add_x_method(this, x_Locate, 'coll')
	add_x_method(this, x_Locate_, '...')

	# -------- M ------- #
	# --- xMaxBy --- #
	add_x_method(this, xMaxBy, 'coll')
	add_x_method(this, xMaxBy_, '...')
	add_x_method(this, x_MaxBy, 'coll')
	add_x_method(this, x_MaxBy_, '...')
	# --- xMinBy --- #
	add_x_method(this, xMinBy, 'coll')
	add_x_method(this, xMinBy_, '...')
	add_x_method(this, x_MinBy, 'coll')
	add_x_method(this, x_MinBy_, '...')
	# --- xMap --- #
	add_x_method(this, xMap, 'coll')
	add_x_method(this, xMap_, '...')
	add_x_method(this, x_Map, 'coll')
	add_x_method(this, x_Map_, '...')

	# --- xMapply --- #
	add_x_method(this, xMapply, 'colls')
	add_x_method(this, xMapply_, '...')
	add_x_method(this, x_Mapply, 'colls')
	add_x_method(this, x_Mapply_, '...')

	# --- xMapIndexed --- #
	add_x_method(this, xMapIndexed, 'coll')
	add_x_method(this, xMapIndexed_, '...')
	add_x_method(this, x_MapIndexed, 'coll')
	add_x_method(this, x_MapIndexed_, '...')

	# -------- N ------- #
	# --- xAddKeys --- #
	add_x_method(this, xAddKeys, 'coll')
	add_x_method(this, x_AddKeys, 'coll')

	# --- xNotMember --- #
	add_x_method(this, xNotMember, 'coll')
	add_x_method(this, xNotMember_, '...')
	add_x_method(this, x_NotMember, 'coll')
	add_x_method(this, x_NotMember_, '...')

	# --- xNotEmpty --- #
	add_x_method(this, xNotEmpty, 'coll')
	add_x_method(this, xNotEmpty_, '...')
	add_x_method(this, x_NotEmpty, 'coll')
	add_x_method(this, x_NotEmpty_, '...')

	# --- xElemNotFalse --- #
	add_x_method(this, xElemNotFalse, 'coll')
	add_x_method(this, xElemNotFalse_, '...')
	add_x_method(this, x_ElemNotFalse, 'coll')
	add_x_method(this, x_ElemNotFalse_, '...')

	# --- xElemNotTrue --- #
	add_x_method(this, xElemNotTrue, 'coll')
	add_x_method(this, xElemNotTrue_, '...')
	add_x_method(this, x_ElemNotTrue, 'coll')
	add_x_method(this, x_ElemNotTrue_, '...')

	# --- xElemNotNa --- #
	add_x_method(this, xElemNotNa, 'coll')
	add_x_method(this, xElemNotNa_, '...')
	add_x_method(this, x_ElemNotNa, 'coll')
	add_x_method(this, x_ElemNotNa_, '...')

	# --- xElemNotNan --- #
	add_x_method(this, xElemNotNan, 'coll')
	add_x_method(this, xElemNotNan_, '...')
	add_x_method(this, x_ElemNotNan, 'coll')
	add_x_method(this, x_ElemNotNan_, '...')

	# --- xElemNotNull --- #
	add_x_method(this, xElemNotNull, 'coll')
	add_x_method(this, xElemNotNull_, '...')
	add_x_method(this, x_ElemNotNull, 'coll')
	add_x_method(this, x_ElemNotNull_, '...')

	# -------- O ------- #
	# --- xOneOf --- #
	add_x_method(this, xOneOf, 'coll')
	add_x_method(this, xOneOf_, '...')
	add_x_method(this, x_OneOf, 'coll')
	add_x_method(this, x_OneOf_, '...')

	# --- xOrderOf --- #
	add_x_method(this, xOrderOf, 'nums')
	add_x_method(this, xOrderOf_, '...')
	add_x_method(this, x_OrderOf, 'nums')
	add_x_method(this, x_OrderOf_, '...')

	# -------- P ------- #
	# --- xPack --- #
	add_x_method(this, xPack, 'coll')
	add_x_method(this, xPack_, '...')
	add_x_method(this, x_Pack, 'coll')
	add_x_method(this, x_Pack_, '...')

	# --- xPoll --- #
	add_x_method(this, xPoll, 'coll')
	add_x_method(this, xPoll_, '...')
	add_x_method(this, x_Poll, 'coll')
	add_x_method(this, x_Poll_, '...')

	# --- xFix --- #
	add_x_method(this, xFix, 'coll')
	add_x_method(this, xFix_, '...')
	add_x_method(this, x_Fix, 'coll')
	add_x_method(this, x_Fix_, '...')

	# --- xPluck --- #
	add_x_method(this, xPluck, 'colls')
	add_x_method(this, xPluck_, '...')
	add_x_method(this, x_Pluck, 'colls')
	add_x_method(this, x_Pluck_, '...')

	# --- xPartition --- #
	add_x_method(this, xPartition, 'coll')
	add_x_method(this, xPartition_, '...')
	add_x_method(this, x_Partition, 'coll')
	add_x_method(this, x_Partition_, '...')

	# --- xReorder --- #
	add_x_method(this, xReorder, 'colls')
	add_x_method(this, xReorder_, '...')
	add_x_method(this, x_Reorder, 'colls')
	add_x_method(this, x_Reorder_, '...')

	# --- xPowerSetOf --- #
	add_x_method(this, xPowerSetOf, 'coll')
	add_x_method(this, xPowerSetOf_, '...')
	add_x_method(this, x_PowerSetOf, 'coll')
	add_x_method(this, x_PowerSetOf_, '...')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xRankOf --- #
	add_x_method(this, xRankOf, 'nums')
	add_x_method(this, xRankOf_, '...')
	add_x_method(this, x_RankOf, 'nums')
	add_x_method(this, x_RankOf_, '...')

	# --- xDeepMap --- #
	add_x_method(this, xDeepMap, 'coll')
	add_x_method(this, xDeepMap_, '...')
	add_x_method(this, x_DeepMap, 'coll')
	add_x_method(this, x_DeepMap_, '...')

	# --- xReduce --- #

	add_x_method(this, xReduce, 'coll')
	add_x_method(this, xReduce_, '...')
	add_x_method(this, x_Reduce, 'coll')
	add_x_method(this, x_Reduce_, '...')

	# --- xRecycle --- #

	add_x_method(this, xRecycle, 'colls')
	add_x_method(this, xRecycle_, '...')
	add_x_method(this, x_Recycle, 'colls')
	add_x_method(this, x_Recycle_, '...')

	# --- xRepeat --- #
	add_x_method(this, xRepeat, 'coll')
	add_x_method(this, xRepeat_, '...')
	add_x_method(this, x_Repeat, 'coll')
	add_x_method(this, x_Repeat_, '...')

	# --- xReject --- #
	add_x_method(this, xReject, 'coll')
	add_x_method(this, xReject_, '...')
	add_x_method(this, x_Reject, 'coll')
	add_x_method(this, x_Reject_, '...')

	# --- xRestOf --- #
	add_x_method(this, xRestOf, 'coll')
	add_x_method(this, xRestOf_, '...')
	add_x_method(this, x_RestOf, 'coll')
	add_x_method(this, x_RestOf_, '...')

	# --- xReverse --- #
	add_x_method(this, xReverse, 'coll')
	add_x_method(this, xReverse_, '...')
	add_x_method(this, x_Reverse, 'coll')
	add_x_method(this, x_Reverse_, '...')

	# --- xRejectNa --- #
	add_x_method(this, xRejectNa, 'coll')
	add_x_method(this, xRejectNa_, '...')
	add_x_method(this, x_RejectNa, 'coll')
	add_x_method(this, x_RejectNa_, '...')

	# --- xRejectNan --- #
	add_x_method(this, xRejectNan, 'coll')
	add_x_method(this, xRejectNan_, '...')
	add_x_method(this, x_RejectNan, 'coll')
	add_x_method(this, x_RejectNan_, '...')

	# --- xRejectNull --- #
	add_x_method(this, xRejectNull, 'coll')
	add_x_method(this, xRejectNull_, '...')
	add_x_method(this, x_RejectNull, 'coll')
	add_x_method(this, x_RejectNull_, '...')

	# --- xRejectEmpty --- #
	add_x_method(this, xRejectEmpty, 'coll')
	add_x_method(this, xRejectEmpty_, '...')
	add_x_method(this, x_RejectEmpty, 'coll')
	add_x_method(this, x_RejectEmpty_, '...')

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
	add_x_method(this, xSecondOf_, '...')
	add_x_method(this, x_SecondOf, 'coll')
	add_x_method(this, x_SecondOf_, '...')

	# --- xProdSetOf --- #
	add_x_method(this, xProdSetOf, 'colls')
	add_x_method(this, xProdSetOf_, '...')
	add_x_method(this, x_ProdSetOf, 'colls')
	add_x_method(this, x_ProdSetOf_, '...')

	# --- xChunk --- #
	add_x_method(this, xChunk, 'coll')
	add_x_method(this, xChunk_, '...')
	add_x_method(this, x_Chunk, 'coll')
	add_x_method(this, x_Chunk_, '...')

	# --- xSelect --- #
	add_x_method(this, xSelect, 'coll')
	add_x_method(this, xSelect_, '...')
	add_x_method(this, x_Select, 'coll')
	add_x_method(this, x_Select_, '...')

	# --- xSplitAt--- #
	add_x_method(this, xSplitAt, 'coll')
	add_x_method(this, xSplitAt_, '...')
	add_x_method(this, x_SplitAt, 'coll')
	add_x_method(this, x_SplitAt_, '...')

	# --- xShuffle --- #
	add_x_method(this, xShuffle, 'coll')
	add_x_method(this, xShuffle_, '...')
	add_x_method(this, x_Shuffle, 'coll')
	add_x_method(this, x_Shuffle_, '...')

	# --- xSplitWith --- #
	add_x_method(this, xSplitWith, 'coll')
	add_x_method(this, xSplitWith_, '...')
	add_x_method(this, x_SplitWith, 'coll')
	add_x_method(this, x_SplitWith_, '...')

	# --- xStopwatch --- #
	add_x_method(this, xStopwatch, 'num')
	add_x_method(this, x_Stopwatch, 'num')

	# --- x_SliceString --- #
	add_x_method(this, xSliceString, 'str')
	add_x_method(this, x_SliceString, 'str')

	# --- xSortBy --- #
	add_x_method(this, xSortBy, 'coll')
	add_x_method(this, xSortBy_, '...')
	add_x_method(this, x_SortBy, 'coll')
	add_x_method(this, x_SortBy_, '...')

	# -------- T ------- #
	# --- xTabulate --- #
	add_x_method(this, xTabulate, 'coll')
	add_x_method(this, xTabulate_, '...')
	add_x_method(this, x_Tabulate, 'coll')
	add_x_method(this, x_Tabulate_, '...')


	# --- xTake --- #
	add_x_method(this, xTake, 'coll')
	add_x_method(this, xTake_, '...')
	add_x_method(this, x_Take, 'coll')
	add_x_method(this, x_Take_, '...')

	# --- xTakeWhile --- #
	add_x_method(this, xTakeWhile, 'coll')
	add_x_method(this, xTakeWhile_, '...')
	add_x_method(this, x_TakeWhile, 'coll')
	add_x_method(this, x_TakeWhile_, '...')

	# --- xToChars --- #
	add_x_method(this, xToChars, 'str')
	add_x_method(this, x_ToChars, 'str')

	# --- xToWords --- #
	add_x_method(this, xToWords, 'str')
	add_x_method(this, x_ToWords, 'str')

	# --- xThirdOf --- #
	add_x_method(this, xThirdOf, 'coll')
	add_x_method(this, xThirdOf_, '...')
	add_x_method(this, x_ThirdOf, 'coll')
	add_x_method(this, x_ThirdOf_, '...')

	# -------- U ------- #
	# --- xUnion --- #
	add_x_method(this, xUnionOf, 'colls')
	add_x_method(this, xUnionOf_, '...')
	add_x_method(this, x_UnionOf, 'colls')
	add_x_method(this, x_UnionOf_, '...')

	# --- xUnit --- #
	add_x_method(this, xUnit, 'coll')
	add_x_method(this, x_Unit, 'coll')

	# --- xUniqueOf --- #
	add_x_method(this, xUniqueOf, 'coll')
	add_x_method(this, xUniqueOf_, '...')
	add_x_method(this, x_UniqueOf, 'coll')
	add_x_method(this, x_UniqueOf_, '...')

	# --- xFromLines --- #
	add_x_method(this, xFromLines, 'strs')
	add_x_method(this, xFromLines_, '...')
	add_x_method(this, x_FromLines, 'strs')
	add_x_method(this, x_FromLines_, '...')

	# --- xFromWords --- #
	add_x_method(this, xFromWords, 'strs')
	add_x_method(this, xFromWords_, '...')
	add_x_method(this, x_FromWords, 'strs')
	add_x_method(this, x_FromWords_, '...')

	# --- xUnzipKeys --- #
	add_x_method(this, xUnzipKeys, 'coll')
	add_x_method(this, xUnzipKeys_, '...')
	add_x_method(this, x_UnzipKeys, 'coll')
	add_x_method(this, x_UnzipKeys_, '...')

	# --- xUnzipIndices --- #
	add_x_method(this, xUnzipIndices, 'coll')
	add_x_method(this, xUnzipIndices_, '...')
	add_x_method(this, x_UnzipIndices, 'coll')
	add_x_method(this, x_UnzipIndices_, '...')

 	# -------- V ------- #
	# -------- W ------- #
	# --- xWhere --- #
	add_x_method(this, xWhere, 'bools')
	add_x_method(this, xWhere_, '...')
	add_x_method(this, x_Where, 'bools')
	add_x_method(this, x_Where_, '...')

	# --- xWriteChars --- #
	add_x_method(this, xWriteChars, 'strs')
	add_x_method(this, x_WriteChars, 'strs')

	# --- xWriteLines --- #
	add_x_method(this, xWriteLines, 'strs')
	add_x_method(this, x_WriteLines, 'strs')

	# --- xWriteWords --- #
	add_x_method(this, xWriteWords, 'strs')
	add_x_method(this, x_WriteWords, 'strs')

	# --- xDelay --- #
	add_x_method(this, xDelay, 'num')
	add_x_method(this, x_Delay, 'num')

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #
	# --- xZip --- #
	add_x_method(this, xZip, 'colls')
	add_x_method(this, xZip_, '...')
	add_x_method(this, x_Zip, 'colls')
	add_x_method(this, x_Zip_, '...')

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
	# --- xAny --- #
	add_x_method(this, xAny, 'pred')
	add_x_method(this, x_Any, 'pred')
	add_x_method(this, xAny_, 'pred')
	add_x_method(this, x_Any_, 'pred')

	# --- xAll --- #
	add_x_method(this, xAll, 'pred')
	add_x_method(this, x_All, 'pred')
	add_x_method(this, xAll_, 'pred')
	add_x_method(this, x_All_, 'pred')

	# --- xAsClosure --- #
	add_x_method(this, xAsClosure, 'fn')
	add_x_method(this, x_AsClosure, 'fn')

	# --- xUnspread --- #
	add_x_method(this, xUnspread, 'fn')
	add_x_method(this, x_Unspread, 'fn')

	# --- xSpread --- #
	add_x_method(this, xSpread, 'fn')
	add_x_method(this, x_Spread, 'fn')

	# --- xApply --- #
	add_x_method(this, xApply, 'fn')
	add_x_method(this, xApply_, 'fn')
	add_x_method(this, x_Apply, 'fn')
	add_x_method(this, x_Apply_, 'fn')

	# --- xArityOf --- #
	add_x_method(this, xArityOf, 'fn')
	add_x_method(this, x_ArityOf, 'fn')

	# -------- B ------- #
	# -------- C ------- #
	add_x_method(this, xCompose_, '...')
	add_x_method(this, x_Compose_, '...')
	# -------- D ------- #
	add_x_method(this, xDropWhile, 'pred')
	add_x_method(this, xDropWhile_, 'pred')
	add_x_method(this, x_DropWhile, 'pred')
	add_x_method(this, x_DropWhile_, 'pred')

	add_x_method(this, xDo, 'fn')
	add_x_method(this, x_Do, 'fn')
	add_x_method(this, xDo_, 'fn')
	add_x_method(this, x_Do_, 'fn')

	# -------- E ------- #
	add_x_method(this, xExists, 'pred')
	add_x_method(this, xExists_, 'pred')

	add_x_method(this, x_Exists, 'pred')
	add_x_method(this, x_Exists_, 'pred')
	# -------- F ------- #

	# --- xFlatMap --- #
	add_x_method(this, xFlatMap, 'fn')
	add_x_method(this, xFlatMap_, 'fn')
	add_x_method(this, x_FlatMap, 'fn')
	add_x_method(this, x_FlatMap_, 'fn')

	# --- xForall --- #
	add_x_method(this, xForall, 'pred')
	add_x_method(this, xForall_, 'pred')
	add_x_method(this, x_Forall, 'pred')
	add_x_method(this, x_Forall_, 'pred')

	# --- xFold --- #
	add_x_method(this, xFold, 'fn')
	add_x_method(this, xFold_, 'fn')
	add_x_method(this, x_Fold, 'fn')
	add_x_method(this, x_Fold_, 'fn')

	# --- xFold --- #
	add_x_method(this, xFold, 'fn')
	add_x_method(this, xFold_, 'fn')
	add_x_method(this, x_Fold, 'fn')
	add_x_method(this, x_Fold_, 'fn')

	# --- xScan --- #
	add_x_method(this, xScan, 'fn')
	add_x_method(this, xScan_, 'fn')
	add_x_method(this, x_Scan, 'fn')
	add_x_method(this, x_Scan_, 'fn')

	add_x_method(this, xScan, 'fn')
	add_x_method(this, xScan_, 'fn')
	add_x_method(this, x_Scan, 'fn')
	add_x_method(this, x_Scan_, 'fn')

	# --- xFormalsOf --- #
	add_x_method(this, xFormalsOf, 'fn')
	add_x_method(this, x_FormalsOf, 'fn')

	# -------- G ------- #
	# -------- G ------- #
	add_x_method(this, xGroupBy, 'fn')
	add_x_method(this, xGroupBy_, 'fn')
	add_x_method(this, x_GroupBy, 'fn')
	add_x_method(this, x_GroupBy_, 'fn')
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
	add_x_method(this, xJuxtapose_, '...')
	add_x_method(this, x_Juxtapose_, '...')

	# -------- K ------- #

	# -------- L ------- #
	# --- xLimit --- #
	add_x_method(this, xLimit, 'fn')
	add_x_method(this, x_Limit, 'fn')

	# --- xLift --- #
	add_x_method(this, xLift, 'fn')
	add_x_method(this, xLift_, '...')
	add_x_method(this, x_Lift, 'fn')
	add_x_method(this, x_Lift_, '...')

	# --- xLocate --- #
	add_x_method(this, xLocate, 'pred')
	add_x_method(this, xLocate_, 'pred')
	add_x_method(this, x_Locate, 'pred')
	add_x_method(this, x_Locate_, 'pred')

	# -------- M ------- #
	# --- xMaxBy --- #
	add_x_method(this, xMaxBy, 'fn')
	add_x_method(this, xMaxBy_, 'fn')
	add_x_method(this, x_MaxBy, 'fn')
	add_x_method(this, x_MaxBy_, 'fn')
	# --- xMinBy --- #
	add_x_method(this, xMinBy, 'fn')
	add_x_method(this, xMinBy_, 'fn')
	add_x_method(this, x_MinBy, 'fn')
	add_x_method(this, x_MinBy_, 'fn')
	# --- xMap --- #
	add_x_method(this, xMap, 'fn')
	add_x_method(this, xMap_, 'fn')
	add_x_method(this, x_Map, 'fn')
	add_x_method(this, x_Map_, 'fn')

	# --- xMapply --- #
	add_x_method(this, xMapply, 'fn')
	add_x_method(this, xMapply_, 'fn')
	add_x_method(this, x_Mapply, 'fn')
	add_x_method(this, x_Mapply_, 'fn')

	# --- xMapIndexed --- #
	add_x_method(this, xMapIndexed, 'fn')
	add_x_method(this, xMapIndexed_, 'fn')
	add_x_method(this, x_MapIndexed, 'fn')
	add_x_method(this, x_MapIndexed_, 'fn')

	# -------- N ------- #
	add_x_method(this, xNot, 'pred')
	add_x_method(this, x_Not, 'pred')

	# -------- O ------- #
	# -------- P ------- #
	# --- xPartition --- #
	add_x_method(this, xPartition, 'pred')
	add_x_method(this, xPartition_, 'pred')
	add_x_method(this, x_Partition, 'pred')
	add_x_method(this, x_Partition_, 'pred')

	# --- xParamsOf --- #
	add_x_method(this, xParamsOf, 'fn')
	add_x_method(this, x_ParamsOf, 'fn')

	# --- xFix --- #
	add_x_method(this, xFix, 'fn')
	add_x_method(this, xFix_, 'fn')
	add_x_method(this, x_Fix, 'fn')
	add_x_method(this, x_Fix_, 'fn')

	# --- xPoll --- #
	add_x_method(this, xPoll, 'pred')
	add_x_method(this, xPoll_, 'pred')
	add_x_method(this, x_Poll, 'pred')
	add_x_method(this, x_Poll_, 'pred')

	# -------- Q ------- #
	# -------- R ------- #
	# --- xDeepMap --- #
	add_x_method(this, xDeepMap, 'fn')
	add_x_method(this, xDeepMap_, 'fn')
	add_x_method(this, x_DeepMap, 'fn')
	add_x_method(this, x_DeepMap_, 'fn')

	# --- xReduce --- #

	add_x_method(this, xReduce, 'fn')
	add_x_method(this, xReduce_, 'fn')
	add_x_method(this, x_Reduce, 'fn')
	add_x_method(this, x_Reduce_, 'fn')

	# --- xReject --- #
	add_x_method(this, xReject, 'pred')
	add_x_method(this, xReject_, 'pred')
	add_x_method(this, x_Reject, 'pred')
	add_x_method(this, x_Reject_, 'pred')

	# -------- S ------- #
	# --- xSelect --- #
	add_x_method(this, xSelect, 'pred')
	add_x_method(this, xSelect_, 'pred')
	add_x_method(this, x_Select, 'pred')
	add_x_method(this, x_Select_, 'pred')

	# --- xSortBy --- #
	add_x_method(this, xSortBy, 'fn')
	add_x_method(this, xSortBy_, '...')
	add_x_method(this, x_SortBy, 'fn')
	add_x_method(this, x_SortBy_, '...')

	# --- xSplitWith --- #
	add_x_method(this, xSplitWith, 'pred')
	add_x_method(this, xSplitWith_, 'pred')
	add_x_method(this, x_SplitWith, 'pred')
	add_x_method(this, x_SplitWith_, 'pred')

	# -------- T ------- #
	# --- xTakeWhile --- #
	add_x_method(this, xTakeWhile, 'pred')
	add_x_method(this, xTakeWhile_, 'pred')
	add_x_method(this, x_TakeWhile, 'pred')
	add_x_method(this, x_TakeWhile_, 'pred')

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
#'    The two primary groups of methods are collection methods and function methods.
#'
#'    Matrices, data frames, and factors have methods for converting them to collections,
#'    while normal Arrow functions are also available as methods for collections
#'    and functions.
#'
#' @section Corner Cases:
#'    The methods that can be used by \bold{$ x_( )} object varies depending
#'    on the type of val.
#'
#' @family methods
#'
#' @example
#'    inst/examples/example-x_.R
#'
#' @export

x_ <- MakeFun(function (val) {
	# Collection any -> Arrow any
	# type constructor for the method-chaining data type.

	MACRO( Must $ Not_Be_Missing(val) )

	if ('arrow' %in% class(val)) {
		val
	} else {
		structure(list(x = val), class = 'arrow')
	}
})

get_proto_ref <- local({

	x_fn_members <-
		ls(x_fn_proto)
	x_matrix_members <-
		ls(x_matrix_proto)
	x_coll_members <-
		ls(x_coll_proto)
	x_data_frame_members <-
		ls(x_data_frame_proto)
	x_data_frame_members <-
		ls(x_data_frame_proto)
	x_factor_members <-
		ls(x_factor_proto)
	x_any_members <-
		ls(x_any_proto)

	function (val) {
		# get the reference to the appropriate methods.

		proto_ref <-
		if (is.function( val )) {
			list(x_fn_proto, x_fn_members)
		} else if (is.matrix( val )) {
			list(x_matrix_proto, x_matrix_members)
		} else if (is.data.frame( val )) {
			list(x_data_frame_proto, x_data_frame_members)
		} else  if (is.atomic( val ) || is.list( val ) ||is.pairlist( val )){
			list(x_coll_proto, x_coll_members)
		} else  if (is.factor( val )) {
			list(x_factor_proto, x_factor_members)
		} else {
			list(x_any_proto, x_any_members)
		}
	}

})


#' @method $ arrow
#' @export



`$.arrow` <- local({

	# some methods are known by their more common
	# but worse names (like filter, filterNot).
	# Meet the user half way and suggest the "proper" name.

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
		alias('xAsNumeric', 	'xAsDouble'),

		alias('xAsChars',		'xToChars'),
		alias('xAsWords', 		'xToWords'),
		alias('xAsLines', 		'xToLines'),

		alias('xToChars', 		'xFromChars'),
		alias('xToWords', 		'xFromWords'),
		alias('xToLines', 		'xFromLines'),

		alias('xByColkeys', 	'xByColrows'),
		alias('xByRowkeys', 	'xByRowrows'),
		alias('xAddNames', 		'xAddKeys'),

		alias('xC', 			'xJoin'),
		alias('xConcat', 		'xJoin'),
		alias('xConcatenate', 	'xJoin'),

		alias('xFilter', 		'xSelect'),
		alias('xFilterNot',     'xReject'),

		alias('xGroup', 		'xChunk'),
		alias('xZipWith', 		'xMapMany')
	)

	suggest_similar_method <- local({

		message <- function (name, contents_are, similar) {

			if (length(similar) == 0) {
				"could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+% "."
			} else {
				"could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+%
				":\n" %+%
				colourise$green("did you mean " %+% sample(similar, size = 1) %+% "?")
			}
		}

		function (val, method_name, contents_are, invoking_call) {
			# given an incorrect method name throw an error
			# suggesting a similar

			proto <- get_proto_ref(val)
			method_name <- method_name

			candidate_methods <- setdiff(proto[[2]], 'private')
			distances <- adist(method_name, candidate_methods)

			similar <- if (method_name %in% names(autosuggested)) {
				autosuggested[[method_name]]
			} else if (min(distances) < nchar(method_name) / 2) {

				candidate_methods[which.min(distances)]

			} else {
				character(0)
			}

			write_error(
				message(method_name, contents_are, similar),
				call. = False)
		}
	})



	function (obj, method) {
		# Arrow a -> symbol -> function
		# return an arrow method associated with the type a.

		method_name <- paste0(match.call()$method)

		proto <- get_proto_ref( obj[['x']] )

		if (method_name %!in% proto[[2]] || method_name == "private") {
			# the invoked method wasn't found,
			# so we should give a suggestion.

			invoking_call <- paste0('$', method_name)

			contents_are <- proto[[1]][['private']][['contents_are']]

			suggest_similar_method(
				obj[['x']], method_name, contents_are, invoking_call)

		}

		fn <- proto[[1]][[method_name]]
		environment(fn)[['Self']] <- function () obj[['x']]

		fn
	}
})

# -------------------------------- Print Method -------------------------------- #

#' @method print arrow
#' @export

print.arrow <- function (x, ...) {
	# custom print statement for the arrow object.

	proto <- get_proto_ref( x[['x']] )
	contents_are <- proto[[1]][['private']] [['contents_are']]

	single_newline <- '\n'
	double_newline <- '\n\n'

	header <- colourise$blue(
		'[ an arrow object with methods for ' %+% contents_are %+% ' ]')

	cat(
		header  %+% double_newline %+%
		'$x_()' %+% single_newline)

	print(x$x_(), ...)
}
