
require (needy)

xAutoPartial <- function (fn) {
	# function -> function
	# takes a n argument function to a function
	# that can take less than n arguments at a time, 
	# eventually passing n arguments to fn.

	if ( '...' %in% names(formals(fn)) ) {
		fn
	} else {
		acc_apply( list(
			fixed = list(), 
			fn = match.fun(fn)) )
	}
}

acc_apply <- function (this) {
	# list -> function | any

	do.call('function', list(
		as.pairlist(xFormals(this$fn)), quote({
			# --- an accumulator function, wrapping 
			# an underlying function.

			# --- Act One: capture function call information,
			# to be added to a new accumulator later.
			this$def <- sys.function()
			# --- for the moment, let's be lazy.
			this$args <- 
				as.list( match.call(this$def, sys.call()) )[-1]
			this$pframe <- parent.frame()

			if (length(this$args) == 0) {
				this$def
			} else {
				# --- Act Two: construct a new accumulator,
				# with more arguments fixed.
				acc <- new_acc(this)
				this <- environment(acc)$this

				# --- Act Three: If the accumulator is full,
				#  call the underlying function. Or return accumulate.

				if (xArity(acc) == 0) {
					do.call(this$fn, this$fixed)
				} else {
					acc
				}
			}
		})
	))
}

new_acc <- function (this) {
	# list -> function
	# return a function, with an environment containing the value
	# 'this', inhereting from the parent frame of xAutoPartial.
	# update the fixed arguments from those in 'this'.

	acc <- this$def
	environment(acc) <- ( function() {

		# --- set to intermediate acc. environment
		new_obj <- new.env(parent = this$pframe)
		# --- add the important fields from this.
		new_obj$this <- list(
			fixed =
				c(this$fixed, this$args),
			fn = 
				this$fn)
		new_obj
	} )()

	# --- switch to the new accumulator's 
	# definition of this.
	
	this <- environment(acc)$this
	formals(acc) <- 
		xFormals(this$fn)[
			setdiff(
				names(formals(this$fn)),
				names(this$fixed)) 
		]
	acc
}
