
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

r_ <- function (get, set) {
	out <- list(
		set = set,
		get = get
	)

	class(out) <- 'rlens'
	out
}







rFirstOf <- r_(
	set = function (coll, val) {
		coll[[1]] <- val
		coll
	},
	get = function (coll) {
		coll[[1]]
	}
)

# a lens factory, not a lens. Should the syntax be different?

rAt <- function (num) {

	r_(
		set = function (coll, val) {
			coll[[num]] <- val
			coll
		},
		get = function (coll) {
			coll[[num]]
		}
	)

}

rTenthOf <- rAt(10)





# lens composition. the pipe op for lenses? lenses need to be
# composed for nested access, so probably.

`*.rlens` <- function (lens1, lens2) {
	r_(
		lens1 $ set %then% lens2 $ set,
		lens1 $ get %then% lens2 $ get
	)
}





# y_( coll ) $ at(1) $ at(1) $ map(100)
#
#
#
#
# ref. <- ..( coll ) $ 1 $ 2










rSelect <- function (pred) {

	list(
		get = coll := xSelect(pred, coll),
		set = (coll1 : coll2) := {
			coll1[xLocate(pred, coll1)] <- coll2
			coll1
		}
	)

}

setDuplicate <- function (lens) {

	list(
		get = lens $ get,
		set = (coll : val) := {
			lens $ set( coll, xRepeat( xLenOf(lens $ get(coll)), val ))

		}
	)

}

setRecycle <- function (lens) {

	list(
		get = lens $ get,
		set = (coll1 : coll2) := {

			lens $ set(coll1, xRecycle( xLenOf(lens $ get(coll1)), coll2))

		}
	)

}

#r = setRecycle(rSelect(x := x %% 2 == 0))
#r $ set(list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 'number')


# ..( 1:10 ) $ r_select(x := x %% 2 == 0) $ r_set $ r_duplicate(1)
