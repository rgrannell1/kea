
message('xCollapse')

forall(
	"collapsing with character() is the same as collapsing with ''",
	list(words = G$words()),
	{

		do.call( xCollapse, c(str = "", list(words)) ) %equals%
		do.call( xCollapse, c(str = list(character()), list(words)) )
	}
)

forall(
	"collapsing character() and '' acts as identity ",
	list(delim = G$word(), words = G$words()),
	{
		stripped_words <-
			Filter(function (x) nchar(x) > 0 && length(x) > 0, words)

		do.call( xCollapse, c(delim, list(words)) ) %equals%
		do.call( xCollapse, c(delim, list(stripped_words)) )
	}
)

message('Arrow $ xCollapse')

