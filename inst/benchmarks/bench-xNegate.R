list(
	truth       = function (x) TRUE,
	falsity     = function (x) FALSE,
	irrelevance = function (x) NA
)



xNegate(truth)(1)
xNegate(falsity)(1)
xNegate(irrelevance)(1)
