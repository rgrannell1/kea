
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
		"xChoose",
		"xCycle",
		"xPermute",
		"xProdSetOf",
		"xPowerSetOf"
	)
)

sd_section(
	"Collection Comprehensions",
	paste(
		"Collection comprehensions provide syntactic sugar for defining collections by providing the contraints",
		"that each element must satisfy to be a member of the collection. These are a translation of set-builder",
		"notation, which defines a set in terms of membership constraints."
	),
	c(
		"xList"
	)
)

sd_section(
	"Container Conversion Functions",
	paste(
		"These functions convert the type of container that surrounds a data set."
	),
	c(
		"xAsCharacter",
		"xAsComplex",
		"xAsDouble",
		"xAsInteger",
		"xAsLogical",
		"xAsRaw"
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
	"Immutable Value Functions",
	paste(
		"These functions work with immutable values; references to a",
		"variable that cannot be altered by reassignment after creation."
	),
	c(
		"xAsVal",
		"xAsVar",
		"xIsVal",
		"xVal"
	)
)

sd_section(
	"Impure Functions",
	paste(
		"These functions are referentially impure or have side-effects",
		"like interacting with the file system."
	),
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
		"xAsDataFrame",
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
	"Basic Functions",
	paste(
		"These very simple functions are useful for function corner cases."
	),
	c(
		"xCapture",
		"xFalsity",
		"xIdentity",
		"xIrrelevance",
		"xTruth"
	)
)

sd_section(
	"Mapping Functions",
	"These functions are variants of map.",
	c(
		"xDeepMap",
		"xFlatMap",
		"xMap",
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
		"xScanl"
	)
)

sd_section(
	"Function Combining Functions",
	"",
	c(
		"xCompose",
		"xLift"
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
	"Name Functions",
	"",
	c(
		"xAsNamed",
		"xGetKey",
		"xPluck",
		"xUnzipNames",
		"xZipNames"
	)
)

sd_section(
	"Order Functions",
	"These functions deal with the order of a collection.",
	c(
		"xOrderOf",
		"xRankOf"
	)
)

sd_section(
	"Parametre Functions",
	"These functions work with or alter function parametres.",
	c(
		"xArityOf",
		"xAsUnary", "xAsVariadic",
		"xIsVariadic",
		"xFormalsOf",
		"xParamsOf",
		"xPartial"
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
		"xLastOf",
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
	"Set Functions",
	"",
	c(
		"xDuplicatesOf",
		"xInter",
		"xIsMember",
		"xNotMember",
		"xUnionOf",
		"xUniqueOf"
	)
)

sd_section(
	"Short-Circuiting Functions",
	paste(
		"These functions can use to the short-circuit operator to",
		"break evaluation prematurely."
	),
	c(
		"xFold", "xFoldr",
		"xReduce", "xReducer",
		"xScanl",
		"xIterate"
	)
)

sd_section(
	"Time Functions",
	paste(
		"These side-effectful functions work with time, or",
		"alter the temporal properties of functions. These are useful for",
		"interacting with rate-limited API's, or allowing programs to",
		"observer their own run-times."
	),
	c(
		"xDelay",
		"xLimit",
		"xStopwatch"
	)
)

sd_section(
	"Value Testing Functions",
	"These functions test values to see if they are a particular type of value.",
	c(
		"xIsEmpty",
		"xIsFalse",
		"xIsNa",
		"xIsNan",
		"xIsNull",
		"xIsTrue",
		"xNotEmpty",
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
