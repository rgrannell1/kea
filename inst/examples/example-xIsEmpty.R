
# 1.
# xIsEmpty is used to check for a recursive function's base case below.

map <- function (fn, coll) {

	if (xIsEmpty(coll)) {
		list()
	} else {
		xJoin...(
			fn(xFirstOf(coll)),
			map(fn, xRestOf(coll)) )
	}
}

map(sqrt, 1:4)

# list(1, 1.4142135623731, 1.73205080756888, 2)

# 2.
# xIsEmpty can be used as a predicate for reject.

xReject...(xIsEmpty, 'a', 'b', integer(0), 'c')

# list('a', 'b', 'c')
