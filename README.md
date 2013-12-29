Arrow v0.1.0
-----------------------------------

<img src="logo.png" height = "180"> </img>

**DISCLAIMER: Arrow is in heavy development and is highly liable to change.**

**Arrow** is a functional programming framework that adds partial application,
jQuery-like method chaining, function composition,
and over one-hundred higher-order- and utility-functions to the R language.
Arrow helps make R an elegant functional language with powerful operations on collections.

## 0 Goal

R is an excellent statistical platform, but it isn't a great general purpose programming language.
Libraries like ```plyr``` and ```stringr``` help, but they don't go far enough. I've
always felt that the best aspects of the R language are its powerful functions, vectorisation and
list data structure. Arrow is an attempt to build upon these aspects of R and to generalise
them into a pleasant, composable functional language.

## 1 Installation

As of late September 2013 **Arrow** is only available on Github. To install the development version, copy the
following into an R console.

```javascript
install.packages('devtools')
require(devtools)

install_github("arrow", "rgrannell1", "develop")
require(devtools)

# check that arrow installed.
xVersion()
```
**Arrow** functions are usually prefixed with the letter 'x'.

## 2 Examples

## 3 Help

To get the documentation for a particular function from an R console simply prefix that function with a question mark.

```
?xSelect
```

## 4 Contributing

I'm (as of November 2013) working on **Arrow** alone. However, the library is very large and I feel that it
would benefit from community contribution, ranging from feedback to active development.

#### 4.1 Library Development
* developing a stable function decorator that behaves like haskell's autocurrying.
* improving efficiency and composability of functions.
* improving exception handing within **Arrow**.

#### 4.2 Tools Development
* Improving the ```forall``` testing tool, and test case generators.
* Improving the ```time_profile``` benchmarking tool.
* Contributing to ```roxygen2``` and ```devtools```, which **Arrow** currently uses.

#### 4.3 Miscelleneous Tasks
* consistency and idiomaticy police; are there any corner cases that need tweaking, or functions that need renaming.
* improving tests, benchmarks and documentation (obviously!)
* suggestions for functions to implement.
* general feedback.
* filing bug reports.

For information about contributing, send me a message at @RyanGrannell on twitter.

#### 4.4 Design
* setting up a Github page for **Arrow**, possibly using ``staticdocs`` and bootstrap.
* designing a logo for **Arrow**.


## 5 Authors

Ryan Grannell.

## 6 Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="gpl3.png" height = "180"> </img>

