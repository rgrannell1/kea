
list(
	one_hundred  = structure(1:100, names = rep('a', 100)),
	one_thousand = structure(1:1000, names = rep('a', 1000)),
	ten_thousand = structure(1:10000, names = rep('a', 10000))
)

xKeysOf(one_hundred)
xKeysOf(one_thousand)
xKeysOf(ten_thousand)
