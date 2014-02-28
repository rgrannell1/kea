
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
	paste(
		"These functions generate combinatorial structures,",
		"which are typically used for specialised patterns of iteration",
		"or generating more useful substructures from an initial pool of",
		"candidate structures. As these functions enumerate rapidly growing",
		"data structures they tend to inefficient for large inputs."
	),
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
	paste(
		"These side-effectful functions work with time, or ",
		"alter the temporal properties of functions. These are useful for",
		"interacting with rate-limited API's."
	),
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
	paste(
		"These functions can use to the short-circuit operator to",
		"break evaluation prematurely."
	),
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
		"xRecycle",
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
	paste(
		"These functions create a subset of collection, with elements sharing a",
		"certain property."
	),
	c(
		"xPartition",
		"xRejectEmpty",
		"xRejectNan",
		"xRejectNa",
		"xRejectNull",
		"xReject",
		"xSelect"
	)
)

sd_section(
	"Value Testing Functions",
	"These functions test values to see if they are a particular type of value.",
	c(
		"xIsFalse",
		"xIsNa",
		"xIsNan",
		"xIsNull",
		"xIsTrue",
		"xNotFalse",
		"xNotNan",
		"xNotNa",
		"xNotNull",
		"xNotTrue",
		"xElemIsFalse",
		"xElemIsNa",
		"xElemIsNan",
		"xElemIsNull",
		"xElemIsTrue",
		"xElemNotFalse",
		"xElemNotNa",
		"xElemNotNan",
		"xElemNotNull",
		"xElemNotTrue"
	)
)


sd_section(
	"Logical Constant Functions",
	paste(
		"These functions are constant functions that return a fixed logical value.",
		"They serve a similar purpose to the identity function."
	),
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
		"xAt",
		"xAtCol",
		"xDropWhile",
		"xDrop",
		"xFirstOf",
		"xFourthOf",
		"xGetKey",
		"xInitOf",
		"xPluck",
		"xRestOf",
		"xSlice",
		"xSecondOf",
		"xTake",
		"xTakeWhile",
		"xThirdOf"
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
	"These functions work with or alter function parametres.",
	c(
		"xArityOf",
		"xAsUnary", "xAsVariadic",
		"xIsVariadic",
		"xParamsOf",
		"xPartial"
	)
)

sd_section(
	"Immutable Value Functions",
	paste(
		"These functions work with immutable values; references to a",
		"variable that cannot be altered by reassignment after creation."
	),
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
		"xCompose",
		"xAsClosure",
		"xAsVariadic",
		"xCapture",
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
	paste0(
		"These functions count or summarise the true/false cases of a predicate",
		" over a data set."
	),
	c(
		"xForall",
		"xExists",
		"xPoll"
	)
)

sd_section(
	"Set Functions",
	"",
	c(
		"xDuplicatesOf",
		"xInterssss"
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
	paste(
		"These functions are involved in text-processing, or interacting",
		"with text-files."
	),
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
