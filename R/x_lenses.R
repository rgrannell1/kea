
# The identity lens
#

y_ <- MakeFun(function (val) {

	out <- function () {
		list(
			get = function () val,
			set = function (val) val
		)
	}

	class(out) <- 'y_'
	out

})





`$.y_` <- function (outer, inner) {

	print(sys.call())

	if (is.character(inner)) {
		inner <- functionLens(match.fun(inner))
	}

	lcompose(outer, inner)

}





lcompose <- '%andThen%' <- function (outer, inner) {

	inner <- eval(body(inner))
	outer <- eval(body(outer))

	function () {

		list(
			get = function (target) {
				inner $ get(outer $ get(target))
			},
			set = function (target, replacement) {
				outer $ set(target, inner $ set(outer $ get(target), replacement))
			}
		)

	}

}

functionLens <-
	function (fn) {
		function () {
			list(
				get = identity,
				set = identity
			)
		}
	}


firstOf <- function () {

	list(
		get = function (coll) coll[[1]],
		set = function (coll, val) {
			coll[[1]] = val
			coll
		}
	)

}

secondOf <- function () {
	list(
		get = function (coll) coll[[2]],
		set = function (coll, val) {
			coll[[2]] = val
			coll
		}
	)
}



lens <- firstOf %andThen% secondOf %andThen% functionLens(paste)





coll <- list(list(1, 2, 3), 4, 5)
#lens $ get(coll)
