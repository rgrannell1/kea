
#
#        Lenses
#
#
# This is an experimental implementation of the lens data types for Kea.
# Ideally, they will take Kea from being awesome for flat or simple collections
# to being awesome for more complex record-type data.
#
# Just a test for now, all syntax is undecided and all abstractions may be
# re-abstracted.

# People like references. Ditch the lens metaphor, and just call lenses
# 'functional references'.

y_(coll) $ .firstOf() $ .secondOf() $ x_Map(sqrt)


#y_ <- function (val) {
#
#	out <- list(
#		get = function () {
#			val
#		},
#		set = function (replacement) {
#			replacement
#		}
#	)
#
#	class(out) <- 'y_'
#	out
#
#}
#
#`$.y_` <- function (lens, lens_name) {
#
#	if (grepl('^[.]', lens_name)) {
#
#	} else {
#
#		fn <- match.fun(lens_name)
#
#		environment(fn) $ Self <- function () {
#			lens[['get']]()
#		}
#
#		fn
#	}
#
#}
#
#.firstOf <- function (target) {
#
#	list(
#		get = function (target) {
#			target[[1]]
#		},
#		set = function (target) {
#			target[[1]] <- replacement
#			target
#		}
#	)
#
#}
#
#map <- function (fn) {
#	lapply(Self(), fn )
#}
#
#
#y_(1:10) $ map(sqrt)