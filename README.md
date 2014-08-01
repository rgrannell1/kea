
Kiwi 0.40.0 [![Build Status](https://travis-ci.org/rgrannell1/kiwi.png)](https://travis-ci.org/rgrannell1/kiwi)
-----------------------------------

> *'By relieving the brain of all unnecessary work, a good notation sets it free to concentrate on more advanced problems, and, in effect, increases the mental power of the race.' -- Alfred N. Whitehead*

Kiwi is a functional programming library built to do three things:

* maximise developer happiness.
* be quick to write and debug.
* make writing large programs easy.

For more information, see Kiwi's [release notice](rgrannell1.github.io).

### Installation

For most people, the best way to get Kiwi is through github.

```splus
if (!require(devtools)) {
    install.packages("devtools"); library(devtools)
}

devtools::install_github("rgrannell1/kiwi", ref = 'releases')
```

Go [here](https://rgrannell1.github.io/kiwi/) for library documentation and tutorials.

Go [here](https://github.com/rgrannell1/kiwi-snippets) for Sublime Text 3 snippets.

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

## Licensing

**Kiwi** is released under the terms of the GNU General Public License version 3.

<img src="https://raw.githubusercontent.com/rgrannell1/kiwi/develop/gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

| Author                      | Changes                 |
| --------------------------- | ----------------------- |
| Ryan Grannell               | 191,792 ++ / 160,881 -- |

