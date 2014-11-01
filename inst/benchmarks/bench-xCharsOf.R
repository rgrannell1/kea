list(
	empty_string = '',
	one_hundred  = paste0(rep('a', 100), collapse = ''),
	one_thousand = paste0(rep('a', 1000), collapse = ''),
	ten_thousand = paste0(rep('a', 10000), collapse = '')
)

xCharsOf(empty_string)
xCharsOf(one_hundred)
xCharsOf(one_thousand)
xCharsOf(ten_thousand)
