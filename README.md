Arrow v0.1
-----------------------------------

**Arrow** is a functional framework that adds dozens of higher-order functions 
and utility functions to the R language. 

## Installation

As of late August 2013 **Arrow** is only available on Github. To install the development version, copy the
following to an R console.

```javascript
install.packages('devtools')
require(devtools)

install_github("arrow", "rgrannell1", "develop")
require(devtools)

# check that arrow installed.
xVersion()
```
All **Arrow** functions are prefixed with the letter "x". This is to avoid naming conflicts and to 
help the user find the function they are looking for.

## Features



### Generic & Idiomatic

In general **Arrow** functions are as generic with respect to input type as possible,
though their output types are more rigidly defined.

```javascript
identical(
    xMap( function (x) x, 1:10),
    xMap( function (x) x, as.list(1:10)) )
identical(
    xMap( function (x) x, 1:10),
    xMap( function (x) x, as.pairlist(1:10)) )

# transitively xMap vector == xMap pairlist
```
Having a fixed output type makes arrow functions very easy to compose.
Refreshingly, it also means that your code won't use ```if(is.na(x))``` like
full stops in an essay.

### Cascading Style

In this style data is fed into the type constructor ```x_```, and methods are called off that object. 
This small program gets every parametre used in the R base library.

```javascript
x_(ls("package:base"))$  
xMap(function (x) get(x))$
xSelect(is.function)$
xMap(xParametres)$
xReducel(union)$
x()
```
The final method - ```x()``` - takes the data out the object constructed by ```x_()``` 
for normal R functions to operate on.

## Licensing

**Arrow** is released under the terms of the GNU General Public License version 3. 

<img src="gpl3.png" height = "180"> </img>
