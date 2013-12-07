
forall <- arrow:::forall
test_cases <- arrow:::test_cases

message('xPermute')

forall(
	"empty list is identity",
	xPermute(list(), list()) %equals% list()
)


message('arrow$xPermute')

