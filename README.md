
Kea 0.47.0 [![Build Status](https://travis-ci.org/rgrannell1/kea.png)](https://travis-ci.org/rgrannell1/kea)
-----------------------------------

> *'By relieving the brain of all unnecessary work, a good notation sets it free to concentrate on more advanced problems, and, in effect, increases the mental power of the race.' -- Alfred N. Whitehead*

Kea is a functional programming library built to do three things:

* maximise developer happiness.
* be quick to write and debug.
* make writing large programs easy.

For more information, see Kea's [release notice](http://rgrannell1.github.io/blog/2014/08/01/introducing-kiwi/).

### Installation

For most people, the best way to get Kea is through github. You must install from the releases branch, as
the developer branch will often be unstable or broken and stable releases are frequent.

```splus
if (!require(devtools)) {
    install.packages("devtools"); library(devtools)
}

devtools::install_github("rgrannell1/kea", ref = 'releases')
```

Go [here](https://rgrannell1.github.io/kea/) for library documentation and tutorials.

Go [here](https://github.com/rgrannell1/kea-snippets) for Sublime Text 3 snippets.

## Examples

```r
# 1. Create a simple csv parser.

from_csv <- xExplode('[ 	]*,[ 	]*')
from_csv("field1", "field2", "field3")

# c("field1, field2, field3")
```

```r
# 2. How many ways can you get one or more royals from a random hand of two cards?

cards <- x__(
	'ace', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'Jack', 'Queen', 'King') $ xRepeat(4)

is_royal <- xIsIn(coll = c('Jack', 'Queen', 'King'))

cards $ xChoose(2) $ xPoll(xAnyOf(is_royal))

# 546 different ways
```

```r
# function composition; print files in your current directory.
xDo(list.files %then% print, '.')
```
```r
# 1. k-mers
#
# A common problem in molecular biology is to find every
# unique continuous substring in a set of
# dna sequences. For example, the spectrum of length
# four continuous substrings in the below sequences

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
			xSliceString((1:k) + ith, string)
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
# "TCG", "CGT", "GTG", "TGA", "GAG", "AGT", "TGC", "TTA", "TAT",
# "AGG", "TGT", "AGA", "GAT")
```



## Licensing

**Kea** is released under the terms of the GNU General Public License version 3.

<img src="https://raw.githubusercontent.com/rgrannell1/kea/develop/gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

| Author                      | Changes                 |
| --------------------------- | ----------------------- |
| Ryan Grannell               | 197,809 ++ / 164,679 -- |

