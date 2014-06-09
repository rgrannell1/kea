

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
# low by only customising the method when it is being called, rather than when the kiwi monad is created.

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
	# -- make a method from an input function.
	# -- side effectfully assign result into environment, since they
	# -- are pass by reference.

	fn_name <- paste0(as.symbol(match.call()$fn))

	# -- normalise the method name from x_Method to xMethos
	fn_sym <- as.symbol(gsub('^x_', 'x', fn_name))

	# -- detect the type of method.
	is_unchaining <- grepl('^x_', fn_name)
	is_variadic   <- grepl('_$', fn_name)

	fn <- match_fn(fn_sym)

	if (!any( fixed == names(formals(fn)) )) {
		stop('not a parametre of ' %+% paste0(fn_sym))
	}

	# -- all parts of this function will be modified.
	method <- function () {	}

	# -- remove the fixed parametre, unless ellipsis is being fixed.
	formals(method) <- if (fixed == '...') {
		formals(fn)
	} else {
		formals(fn)[ names(formals(fn)) != fixed ]
	}

	if (!is_unchaining && !is_variadic) {
		# xMethod

		# -- construct the function body.
		body(method) <-
			bquote({

				x_(.(
					( as.call(c(
						# -- call the function
						fn_sym,
						# -- for each parametre in the (always a closure)
						lapply(
							names(formals(fn)),
							function (param) {

								if (as.symbol(param) == fixed) {
									# -- if this parametre is fixed use
									# -- the invoker Self()
									quote(Self())
								} else {
									# -- use the parametre name.
									as.symbol(param)
								}
							}) )) ) ))
			})

	} else if (is_unchaining && !is_variadic) {
		# x_Method

		# -- construct the function body.
		body(method) <-
			bquote({

				.(
					( as.call(c(
						# -- call the function
						fn_sym,

						lapply(
							names(formals(fn)),
							function (param) {

								if (as.symbol(param) == fixed) {
									# -- if this parametre is fixed use
									# -- the invoker Self()
									quote(Self())
								} else {
									# -- use the parametre name.
									as.symbol(param)
								}
							}) )) ))
			})

	} else if (is_variadic) {

		# -- accumulate a parametres list.
		# -- done with Reduce as more work is needed for variadic formals.
		params <- Reduce(
			function (acc, param) {

				# -- this parametre is to be fixed.

				if (param == fixed) {

					if (fixed == '...') {
						# -- fixing an ellipsis parametre
						c( acc, quote(Self()), as.symbol('...') )
					} else {
						# -- normal fixing
						c( acc, quote(Self()) )
					}

				} else {
					# -- don't fix this parametre.
					c(acc, as.symbol(param))
				}
			},
			names(formals(fn)),
			list()
		)

		# -- set the body for a variadic function.
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

	# -- essential for avoiding closure problems.
	environment(method) <- new.env(parent = environment(fn))

	# -- side-effectful update the environment passed in with a method.
	env[[fn_name]] <- method

}

# -------------------------------- Inheritance -------------------------------- #
#
# There are certain functions that every prototype gets, like identity. These are
# factored into their own prototype. These methods are then inherited by the other
# prototypes (allowing for the possibility of overridding methods in the inheritee).
#
# This might eventually just be a combination of joining the search paths, and
# assigning the private variable manually, but this might make function lookup slower.

inherit_prototypes <- function (parent, child, description) {

	child_methods  <- ls(envir = child)
	parent_methods <- ls(envir = parent)

	# -- methods in parent can be overwritten by the child
	non_overwritten <- setdiff(parent_methods, child_methods)

	# -- fairly slow, so use this function only during building.
	child <- as.environment(c(
		as.list(child),
		as.list(parent)[non_overwritten]
	))

	child $ private <- list(contents_are = description)

	child
}

inherit_x_any <- function (child, description) {
	inherit_prototypes(x_any_proto, child, description)
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

	add_x_method(this, xK, 'val')
	add_x_method(this, x_K, 'val')

	# -------- D ------- #
	# -------- E ------- #
	add_x_method(this, xExecute, 'val')
	add_x_method(this, x_Execute, 'val')

	# -------- F ------- #
	# -------- G ------- #

	# -------- H ------- #
	# -------- I ------- #
	add_x_method(this, xIdentity, 'val')
	add_x_method(this, x_Identity, 'val')

	# --- xIsEmpty --- #
	add_x_method(this, xIsEmpty, 'coll')
	add_x_method(this, xIsEmpty_, '...')
	add_x_method(this, x_IsEmpty, 'coll')
	add_x_method(this, x_IsEmpty_, '...')

	# --- xIsNa --- #
	add_x_method(this, xIsNa, 'val')
	add_x_method(this, x_IsNa, 'val')

	# --- xIsNan --- #
	add_x_method(this, xIsNan, 'val')
	add_x_method(this, x_IsNan, 'val')

	# --- xIsNull --- #
	add_x_method(this, xIsNull, 'val')
	add_x_method(this, x_IsNull, 'val')

	# --- xIsTrue --- #
	add_x_method(this, xIsTrue, 'val')
	add_x_method(this, x_IsTrue, 'val')

	add_x_method(this, xIsFalse, 'val')
	add_x_method(this, x_IsFalse, 'val')

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
	add_x_method(this, xTap, 'val')
	add_x_method(this, x_Tap, 'val')

	add_x_method(this, xThread, 'val')
	add_x_method(this, xThread_, 'val')
	add_x_method(this, x_Thread, 'val')
	add_x_method(this, x_Thread_, 'val')

	# -------- U ------- #
	# -------- V ------- #
	add_x_method(this, xVersion, '...')
	add_x_method(this, x_Version, '...')

	# -------- W ------- #
	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #

	this $ private <- list(contents_are = "arbitrary values")
	this
})










# -------------------------------- Not-quite-a-collection methods -------------------------------- #
#
# methods for non-canonical data types in kiwi; data frames, tables, matrices and other odd and
# sometimes awkward structures. I don't want these to be treated as first-class citizens (particularily data frames),
# but there is no reason they shouldn't have methods to convert them into a list representation.
#

x_matrix_proto <- local({

	this <- Object()

	# -------- A ------- #
	# -------- B ------- #
	add_x_method(this, xByCols, 'colls')
	add_x_method(this, x_ByCols, 'colls')

	add_x_method(this, xByColkeys, 'colls')
	add_x_method(this, x_ByColkeys, 'colls')

	add_x_method(this, xByRows, 'colls')
	add_x_method(this, x_ByRows, 'colls')

	# --- xByRowkeys --- #
	add_x_method(this, xByRowkeys, 'colls')
	add_x_method(this, x_ByRowkeys, 'colls')

	# -------- C ------- #

	# -------- D ------- #
	# -------- E ------- #
	add_x_method(this, xElemsByCols, 'colls')
	add_x_method(this, x_ElemsByCols, 'colls')

	add_x_method(this, xElemsByRows, 'colls')
	add_x_method(this, x_ElemsByRows, 'colls')

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

	inherit_x_any(this, 'matrices')

})






x_data_frame_proto <- local({

	this <- Object()

	# -------- A ------- #

	# -------- B ------- #
	# --- xByCols --- #
	add_x_method(this, xByCols, 'colls')
	add_x_method(this, x_ByCols, 'colls')

	# --- xByColkeys --- #
	add_x_method(this, xByColkeys, 'colls')
	add_x_method(this, x_ByColkeys, 'colls')

	# --- xByRows --- #
	add_x_method(this, xByRows, 'colls')
	add_x_method(this, x_ByRows, 'colls')

	# --- xByRowkeys --- #
	add_x_method(this, xByRowkeys, 'colls')
	add_x_method(this, x_ByRowkeys, 'colls')

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

	inherit_x_any(this, 'data.frames')

})


x_factor_proto <- local({

	this <- Object()

	# -------- A ------- #
	# -------- B ------- #

	add_x_method(this, xByLevels, 'coll')
	add_x_method(this, x_ByLevels, 'coll')

	add_x_method(this, xByValues, 'coll')
	add_x_method(this, x_ByValues, 'coll')

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

	inherit_x_any(this, 'factors')

})



# -------------------------------- Collection methods -------------------------------- #
#
# methods specific to vectors, lists or pairlists.
#
#
x_coll_proto <- local({

	this <- Object()

	# -------- A ------- #
	# --- xAnyOf --- #
	add_x_method(this, xAnyOf, 'coll')
	add_x_method(this, x_AnyOf, 'coll')
	add_x_method(this, xAnyOf_, '...')
	add_x_method(this, x_AnyOf_, '...')

	# --- xAllOf --- #
	add_x_method(this, xAllOf, 'coll')
	add_x_method(this, x_AllOf, 'coll')
	add_x_method(this, xAllOf_, '...')
	add_x_method(this, x_AllOf_, '...')

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
	add_x_method(this, xCycle, 'coll')
	add_x_method(this, xCycle_, '...')
	add_x_method(this, x_Cycle, 'coll')
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

	# --- xExplode --- #
	add_x_method(this, xExplode, 'str')
	add_x_method(this, x_Explode, 'str')

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

	# --- xIsMatch --- #
	add_x_method(this, xIsMatch, 'str')
	add_x_method(this, x_IsMatch, 'str')

	# --- xIterate --- #
	add_x_method(this, xIterate, 'val')
	add_x_method(this, x_Iterate, 'val')
	add_x_method(this, xIs, 'val1')
	add_x_method(this, x_Is, 'val1')
	add_x_method(this, xNot, 'val1')
	add_x_method(this, x_Not, 'val1')

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

	# -------- N ------- #
	# --- xNoneOf --- #
	add_x_method(this, xNoneOf, 'coll')
	add_x_method(this, x_NoneOf, 'coll')
	add_x_method(this, xNoneOf_, '...')
	add_x_method(this, x_NoneOf_, '...')

	# --- xAddKeys --- #
	add_x_method(this, xAddKeys, 'coll')
	add_x_method(this, x_AddKeys, 'coll')

	# --- xNotMatch --- #
	add_x_method(this, xNotMatch, 'str')
	add_x_method(this, x_NotMatch, 'str')

	# --- xNotSubset --- #
	add_x_method(this, xNotSubset, 'coll2')
	add_x_method(this, xNotSubset_, '...')
	add_x_method(this, x_NotSubset, 'coll2')
	add_x_method(this, x_NotSubset_, '...')

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

	add_x_method(this, xSliceString_, 'str')
	add_x_method(this, x_SliceString_, 'str')

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
	add_x_method(this, xUnit_, '...')
	add_x_method(this, x_Unit_, '...')

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

	inherit_x_any(this, 'collections')

})






























# -------------------------------- Function methods -------------------------------- #
#
# These methods operate on functions wrapped in the kiwi object.
# I anticipate that these methods will be less used, but there's no reason to exclude them
# entirely. Methods such as Apply work nicely with this style.






x_fn_proto <- local({

	this <- Object()

	# -------- A ------- #
	# --- xAnyOf --- #
	add_x_method(this, xAnyOf, 'pred')
	add_x_method(this, x_AnyOf, 'pred')
	add_x_method(this, xAnyOf_, 'pred')
	add_x_method(this, x_AnyOf_, 'pred')

	# --- xAllOf --- #
	add_x_method(this, xAllOf, 'pred')
	add_x_method(this, x_AllOf, 'pred')
	add_x_method(this, xAllOf_, 'pred')
	add_x_method(this, x_AllOf_, 'pred')

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
	# -------- F ------- #

	# --- xFlatMap --- #
	add_x_method(this, xFlatMap, 'fn')
	add_x_method(this, xFlatMap_, 'fn')
	add_x_method(this, x_FlatMap, 'fn')
	add_x_method(this, x_FlatMap_, 'fn')


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

	# -------- N ------- #
	add_x_method(this, xNoneOf, 'pred')
	add_x_method(this, x_NoneOf, 'pred')
	add_x_method(this, xNoneOf_, 'pred')
	add_x_method(this, x_NoneOf_, 'pred')

	add_x_method(this, xNegate, 'pred')
	add_x_method(this, x_Negate, 'pred')

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

	# -------- W ------- #

	# --- xDelay --- #
	add_x_method(this, xDelay, 'fn')
	add_x_method(this, x_Delay, 'fn')

	# -------- X ------- #
	# -------- Y ------- #
	# -------- Z ------- #

	inherit_x_any(this, 'functions')

})

# -------------------------------- Type Constructor -------------------------------- #

#' x_
#'
#' Generate an kiwi object with methods available to it.
#'
#' @param
#'    val an arbitrary value. The value to wrap in an
#'    kiwi object.
#'    The methods available depend on the input
#'    type; functions and collections have the most methods available.
#'
#' @param
#'    ... see above.
#'
#' @return
#'    An object of class "kiwi". Internally the object is represented as a
#'    list with a single field \bold{x}, but this field cannot be accessed directly.
#'    Instead, the method \bold{$ x_( )} or \bold{$ x_Identity( )} can be used to
#'    return the data stored in an kiwi object.
#'
#'    The methods available to an kiwi object depend on the type of the data it
#'    contains. All kiwi objects inherit a handful of methods regardless of their
#'    type; these include \bold{xIdentity} and \bold{xTap} - a method that allows
#'    anonymous function to be executed on an kiwi object.
#'
#'    The two primary groups of methods are collection methods and function methods.
#'
#'    Matrices, data frames, and factors have methods for converting them to collections,
#'    while normal Kiwi functions are also available as methods for collections
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
#' @rdname x_
#' @export

x_ <- MakeFun(function (val) {
	# Collection any -> Kiwi any
	# type constructor for the method-chaining data type.

	MACRO( Fix(x_, val) )

	# -- a useful corner case; there are no methods
	# -- specifically for kiwi objects with kiwi
	# -- objects in them. Makes defining methods easier.
	if (any(class(val) == 'kiwi')) {
		val
	} else {
		# -- cannot just be a val with a class label,
		# -- as if val is null then x_ will fail.
		structure(list(x = val), class = 'kiwi')
	}
})











get_proto_ref <- local({

	x_fn_members         <- ls(x_fn_proto)
	x_matrix_members     <- ls(x_matrix_proto)
	x_coll_members       <- ls(x_coll_proto)
	x_data_frame_members <- ls(x_data_frame_proto)
	x_factor_members     <- ls(x_factor_proto)
	x_any_members        <- ls(x_any_proto)

	function (val) {
		# get the reference to the appropriate methods.

		# -- keep this code fairly efficient.

		proto_ref <-
		if (is.function( val )) {
			list(x_fn_proto, x_fn_members)
		} else if (is.matrix( val )) {
			list(x_matrix_proto, x_matrix_members)
		} else if (is.data.frame( val )) {
			list(x_data_frame_proto, x_data_frame_members)
		} else if (is.factor( val )){
			list(x_factor_proto, x_factor_members)
		} else if (is_atomic( val ) || is_generic( val )) {
			list(x_coll_proto, x_coll_members)
		} else {
			list(x_any_proto, x_any_members)
		}
	}

})












#' @rdname x_
#' @export

x__ <- function (...) {
	x_(list(...))
}











#' @export

`$.kiwi` <- local({

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
		alias('xAsNumeric',   'xAsDouble'),

		alias('xAsChars',     'xToChars'),
		alias('xAsWords',     'xToWords'),
		alias('xAsLines',     'xToLines'),

		alias('xToChars',     'xFromChars'),
		alias('xToWords',     'xFromWords'),
		alias('xToLines',     'xFromLines'),

		alias('xByColkeys',   'xByColrows'),
		alias('xByRowkeys',   'xByRowrows'),
		alias('xAddNames',    'xAddKeys'),

		alias('xC',           'xJoin'),
		alias('xConcat',      'xJoin'),
		alias('xConcatenate', 'xJoin'),

		alias('xFilter',      'xSelect'),
		alias('xFilterNot',   'xReject'),

		alias('xGroup',       'xChunk'),
		alias('xZipWith',     'xMapMany')
	)

	suggest_similar_method <- local({
		# -- the spell-checker for Kiwi's methods.

		message <- function (name, contents_are, similar) {

			if (length(similar) == 0) {
				"Could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+% "."
			} else {
				"Could not find the method " %+% dQuote(name) %+%
				" in the methods available for " %+% contents_are %+%
				":\n" %+%
				colourise$green("did you mean " %+% rsample(similar, size = 1) %+% "?")
			}
		}

		function (val, method_name, contents_are, invoking_call) {
			# given an incorrect method name throw an error
			# suggesting a similar

			proto             <- get_proto_ref(val)
			method_name       <- method_name

			candidate_methods <- setdiff(proto[[2]], 'private')

			# -- get the edit distance to each method in the prototype.
			distances <- adist(method_name, candidate_methods)

			similar <- if ( any(method_name == names(autosuggested)) ) {
				autosuggested[[method_name]]
			} else if (min(distances) < nchar(method_name) / 2) {

				candidate_methods[which.min(distances)]

			} else {
				character(0)
			}

			throw_kiwi_error(
				message = message(method_name, contents_are, similar))
		}
	})

	function (obj, method) {
		# Kiwi a -> symbol -> function
		# return an kiwi method associated with the type a.

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


#' @export

print.kiwi <- function (x, ...) {

	proto        <- get_proto_ref( x[['x']] )
	contents_are <- proto[[1]][['private']] [['contents_are']]

	header <- colourise$blue(
		'[ an kiwi object with methods for ' %+% contents_are %+% ' ]')

	cat(
		header  %+% '\n\n' %+%
		'$x_()' %+% '\n')

	print(x $ x_Identity(), ...)
}
