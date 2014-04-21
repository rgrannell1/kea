
# 1. k-mers
#
# A common problem in molecular biology is to find every
# unique continous substringin a set of
# dna sequences. For example, the spectrum of length
# four continous substrings in the below sequences

# contig1: AATTCCGG
# contig2: TGGTAACGG

# is

# AATT, ATTC, TTCC, TCCG, CCGG
# TGGT, GGTA, GTAA, TAAC, AACG, ACGG

# this can be useful when trying to sequence a genome from a
# lot of random overlapping subsequences of the genome. In a non-molecular
# biology context this is just a run of the mill string algorithm.
#
# xSliceString makes it easy to select a subsequence from a string

kmers <- (k : string) := {

	xMap(
		ith := {
			# subset the string.
			xSliceString(string, (1:k) + ith)
		},
		0:(nchar(string) - k)
	)

}

# the DNA sequences to find kmers of.

contigs <- list(
	'AT',
	'AATTCCGG',
	'GGTCGTCGGTGAGTGC',
	'TTATTATTA',
	'A',
	'AGGTGTGAGAGAT'
)

# get all unique 3-mers.
x_(contigs) $ xReject(contig := nchar(contig) < 3) $ # -- remove the contigs that are too short.
xMap(contig := {
	kmers(3, contig)
}) $
xJoin() $ x_UniqueOf() # -- join all the kmers into one flat list, and get the unique ones.




# list("AAT", "ATT", "TTC", "TCC", "CCG", "CGG", "GGT", "GTC",
#     "TCG", "CGT", "GTG", "TGA", "GAG", "AGT", "TGC", "TTA", "TAT",
#     "AGG", "TGT", "AGA", "GAT")
