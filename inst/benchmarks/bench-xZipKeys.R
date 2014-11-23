
list(
	one_hundred  = lapply(1:100,  function (x) list('a', 1L)),
	one_thousand = lapply(1:1000, function (x) list('a', 1L)),
	ten_thousand = lapply(1:1000, function (x) list('a', 1L))
)

xZipKeys(one_hundred)
xZipKeys(one_thousand)
xZipKeys(ten_thousand)
