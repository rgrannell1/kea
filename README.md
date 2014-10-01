
Kea 0.59.0 [![Build Status](https://travis-ci.org/rgrannell1/kea.png)](https://travis-ci.org/rgrannell1/kea)
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

Go [here](https://github.com/rgrannell1/kea-snippets) for Sublime Text 3 snippets.

### Usage

#### Partial Application

Kea functions are partially applicable; they don't require all their arguments be supplied at once.

```r
xIsMatch('[0-9]+')
# -- base R syntax
function (str) xIsMatch('[0-9]+', str)

# -- xIsMatch is given a regular expression, then mapped over some strings.

xMap(xIsMatch('[0-9]+'), c('123', 'abc'))
# list(True, False)
```

#### Methods

Every function has a corresponding method; data is first wrapped with `x_` and
then passed to methods with `$`.

```r
x_(1:10) $ xMap(sqrt) $ x_Reduce('+')
# 22.46828
```










## Licensing

**Kea** is released under the terms of the GNU General Public License version 3.

<img src="https://raw.githubusercontent.com/rgrannell1/kea/develop/gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

Maintained and developed by Ryan Grannell.
