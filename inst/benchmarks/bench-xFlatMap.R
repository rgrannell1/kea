
list(
	one_hundred  = 1:100,
	one_thousand = 1:1000,
	ten_thousand = 1:10000
)

xFlatMap()
xFlatMap(identity)
xFlatMap(identity, one_hundred)
xFlatMap(identity, one_thousand)
xFlatMap(identity, ten_thousand)
