
# -------- fold methods -------- #

fold <- list(
	vector = 
		function (fn, init, coll) {
			# a definition of folds over linear collection.

			fn <- match.fun(fn)

			if (length(coll) == 0) {
				coll
			} else if (length(coll) == 1) {
				coll[[1]]
			} else {
				
				init <- coll[[1]]
				coll <- tail(coll, -1)

				callCC(
					function (SHORT_CIRCUIT) {
						# allow jumping out of reduce early.

						for (ith in seq_along(coll)) {
							init <- fn( init, coll[[ith]] )
						}
						init						
					}
				)
			}
		}
)

# -------- constructors -------- #

Foldable <- function (fold) {
	# return a constructor function for a particular 
	# foldable class

	function (val) {
		
		this <- list(
			x = val,
			unit = if (is.pairlist(val)) {
				Null
			} else {
				val[0]
			},
			fold = fold
		)

		structure(this, class = 'foldable')
	}
}

Vector <- Foldable(fold$vector)

# -------- the $ chaining infix function -------- #

'$.foldable' <- function (obj, method) {
	# lookup the methods, and bind fold_, self_, and unit_ in the
	# methods environment before returning.

	method_name <- paste0(method)
	pcall <- paste0('$', method_name)

	proto_ref <- proto_collection

	assert(
		method_name %in% ls(proto_ref), pcall)

	fn <- proto_ref[[method_name]]

	environment(fn)[['self_']] <- function () obj[['x']]
	environment(fn)[['fold_']] <- function () obj[['fold']]
	environment(fn)[['unit_']] <- function () obj[['unit']]

	fn
}

# -------- methods defined on each collection -------- #

proto_collection <- local({

	this <- new.env(parent = emptyenv())

	this$xPoll <- 
		function (fn) {

			fold_()(
				function (acc, new) {
					if (fn(new)) acc + 1 else acc
				}, 
				0, 
				self_()
			)
		}
	
	this$xMap <-
		function (fn) {
			
			fold_()(
				function (acc, new) c(acc, fn(new)),
				unit_(),
				self_()
			)
		}

	this$xLocate <-
		function (fn) {

			fold_()(
				function (acc, new) {
					if (fn(new)) {
						SHORT_CIRCUIT(acc)
					} else {
						acc + 1
					}
				},
				0,
				self_()
			)
		}

	this
})
