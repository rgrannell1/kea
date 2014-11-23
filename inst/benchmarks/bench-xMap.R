
list(
	one_hundred  = 1:100,
	one_thousand = 1:1000,
	ten_thousand = 1:10000
)

xMap()
xMap(identity)
xMap(identity, one_hundred)
xMap(identity, one_thousand)
xMap(identity, ten_thousand)
