list(
	one_hundred  = rep(FALSE, 100),
	one_thousand = rep(FALSE, 1000),
	ten_thousand = rep(FALSE, 10000)
)

xAnyOf(identity, one_hundred)
xAnyOf(identity, one_thousand)
xAnyOf(identity, ten_thousand)
