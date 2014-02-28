
sd_section(
	"Function Application Functions",
	"These functions apply other functions to data in interesting ways.",
	c(
		"xApply",
		"xAsUnary",
		"xAsVariadic",
		"xThread"
	)
)

sd_section(
	"Combinatoric Functions",
	"These functions generate combinatorial structures.",
	c(
		"xCombos",
		"xCycle",
		"xPermute",
		"xProdSetOf",
		"xPowerSetOf"
	)
)

sd_section(
	"Time Functions",
	"These functions involve timing.",
	c(
		"xDelay",
		"xLimit",
		"xStopwatch"
	)
)


sd_section(
	"Impure Functions",
	paste0(
		"These functions are referentially impure or have side-effects ",
		"like interacting with the file system."),
	c(
		"xDelay",
		"xDo",
		"xExecute",
		"xReadChars",
		"xReadLines",
		"xReadWords",
		"xStopwatch",
		"xVersion",
		"xWriteChars",
		"xWriteWords",
		"xWriteLines"
	)
)

sd_section(
	"Methods",
	"These functions are only available as arrow methods.",
	c(
		"xByColnames",
		"xByCols",
		"xByLevels",
		"xByRows",
		"xColUnit",
		"xElemsByCols",
		"xExecute",
		"xElemsByRows",
		"xRowUnit",
		"xTap",
		"xTranspose",
		"x_"
	)
)

sd_section(
	"Short-Circuiting Functions",
	"These functions use the short circuiting Return function.",
	c(
		"xFold", "xFoldl",
		"xReduce", "xReducel",
		"xScan",
		"xIterate"
	)
)

sd_section(
	"Reshaping Functions",
	"",
	c(
		"xChop",
		"xChunk",
		"xFlatten",
		"xJoin",
		"xOneOf",
		"xRepeat",
		"xReverse",
		"xShuffle",
		"xSortBy",
		"xSplitAt",
		"xSplitBy",
		"xTabulate",
		"xUnzipNames",
		"xZipNames",
		"xZip"
	)
)

sd_section(
	"Filtering Functions",
	"",
	c(
		"xPack",
		"xPartition",
		"xRejectEmpty",
		"xRejectNan",
		"xRejectNa",
		"xRejectNull",
		"xReject"
	)
)

sd_section(
	"Testing Functions",
	"These functions test values to see if they are a particular type of value.",
	c(


	)
)

sd_section(
	"Logical Constant Functions",
	"These functions are constant functions that return a fixed logical value.",
	c(
		"xTruth",
		"xFalsity",
		"xIrrelevance"
	)
)

sd_section(
	"Selection Functions",
	"These functions select or subset collections of values.",
	c(
		"xDropWhile",
		"xDrop",
		"xFirstOf",
		"xFourthOf",
		"xGetKey",
		"xInitOf",
		"xPluck",
		"xRestOf",
		"xSecondOf",
		"xTakeWhile"
	)
)

sd_section(
	"Mapping Functions",
	"These functions are variants of map.",
	c(
		"xDeepMap",
		"xFlatMap",
		"xMapIndexed",
		"xMapply",
		"xVectorise"
	)
)

sd_section(
	"Folding Functions",
	paste0(
		"Folding is a fundemental operation in functional programming. ",
		"These functions are varients of fold."
	),
	c(
		"xFold", "xFoldl",
		"xReduce", "xReducel",
		"xScan"
	)
)

sd_section(
	"Parametre Functions",
	"",
	c(
		"xAsUnary", "xAsVariadic",
		"xIsVariadic", "xParamsOf",
		"xPartial"
	)
)

sd_section(
	"Immutable Value Functions",
	"",
	c(
		"xAsVal",
		"xAsVar",
		"xVal"
	)
)

sd_section(
	"Function Modifying Functions",
	"",
	c(
		"%and%",
		"%or%",
		"%of%",
		"%then%",
		"xCompose",
		"xAsClosure",
		"xAsVariadic",
		"xCapture",
		"xK",
		"xDelay",
		"xJuxtapose",
		"xNot",
		"xPartial",
		"xThread",
		"xVectorise"
	)
)

sd_section(
	"Quantifier Functions",
	"",
	c(
		"xForall",
		"xExists"
	)
)

sd_section(
	"Set Functions",
	"",
	c(

	)
)

sd_section(
	"Name Functions",
	"",
	c(

	)
)

sd_section(
	"Function Combining Functions",
	"",
	c(

	)
)

sd_section(
	"Character Functions",
	"",
	c(
		"xExplode",
		"xFromChars",
		"xFromLines",
		"xFromWords",
		"xImplode",
		"xReadChars",
		"xReadLines",
		"xReadWords",
		"xSubstring",
		"xToChars",
		"xToLines",
		"xToWords",
		"xWriteChars",
		"xWriteLines",
		"xWriteWords"
	)
)
