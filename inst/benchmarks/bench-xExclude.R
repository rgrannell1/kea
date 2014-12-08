
list(
	one_hundred  = 1:100,
	one_thousand = 1:1000,
	ten_thousand = 1:10000
)

xExclude(one_hundred,  one_hundred)
xExclude(one_thousand, one_hundred)
xExclude(ten_thousand, one_hundred)

xExclude(one_hundred,  one_thousand)
xExclude(one_thousand, one_thousand)
xExclude(ten_thousand, one_thousand)

xExclude(one_hundred,  ten_thousand)
xExclude(one_thousand, ten_thousand)
xExclude(ten_thousand, ten_thousand)
