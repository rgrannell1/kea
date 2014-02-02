
#' Usage - Methods
#'
#' A guide to using Arrow methods.
#'
#' Methods are useful if there is clearly defined divisions between
#' subtasks in a program; for example selecting the odd numbers,
#' then squaring them, and then summing the result.
#'
#' \code{
#' x_(1:10) $\cr
#' xSelect(n := n %% 2 == 0) $\cr
#' xMap(n := n^2) $\cr
#' x_Reduce('+')\cr
#' }
#'
#' The above case shows most of the key features of Arrow methods. First
#' the data to be processed is added to the arrow object constructor.
#'
#' \bold{The Constructor Function x_}
#'
#' There isn't much to say about the function \bold{x_( )}; it doesn't actually do very much.
#' \bold{x_( )} takes a datum and attaches the class 'arrow' to it.
#'
#' \code{x_(1:10)$x_()}
#'
#' The data can be taken back out of an arrow object using the \bold{x_( )} method
#' (if I had my way with R it would be called \bold{_x}).
#' It isn't usually necessary to call this function directly, as shown below.
#'
#' \bold{The infix function $}
#'
#' The dollar function returns a function with the data
#' from the arrow object left-hand side of the call (in this
#' case 1...10) bound to a function called \bold{Self}
#'
#' \code{
#' x_(1:10) $ x_Map
#' function (fn)
#' {
#'     x_(xMap(fn, Self()))
#' }
#' }
#'
#' In the above call the method `xMap` for collections is returned. In
#' this case xMap's coll parametre is bound to the value 1...10
#' (represented by the function `Self`). Since xMap's
#' coll parametre is fixed it only has one unfixed parametre: fn.
#'
#' Note that different methods are available depending on the
#' type of the data on the left hand side of the call.
#'
#' \code{x_(sqrt) $ x_Map(coll = 1:10)}
#'
#' In the above call the square root function is given to the
#' \bold{x_( )} constructor, and the x_Map method for *functions* is returned,
#' with its fn parametre fixed as `sqrt`, and its coll parametre free.
#'
#' As of Arrow 0.1, there are methods for matrices, data.frames, factors,
#' lists pairlists and typed vectors, and functions. There are also a handful
#' of methods (such as xIdentity) available to all data types.
#'
#' The dollar function also supplies suggestions if a non-existing method name
#' is supplied to the right-hand side of the call.
#'
#' \bold{Types of Methods}
#'
#' Every arrow method has a chaining and unchaining form. The distinction is simple;
#' chaining methods (xMethod) return arrow objects, whicle unchaining
#' methods (x_Method) return the values inside arrow objects.
#'
#' Unchaining methods are used when you want to return a value you can
#' use with a normal R function.
#'
#' \code{sample(letters, size = x_(letters) $ x_Lenof)}
#'
#' Chaining methods are used when you want to call a method off the result
#' of another method.
#'
#' \code{x_(letters) $ xTake(10) $ x_LenOf()}
#'
#' Variadic method (methods with '...' arguments) with their '...' argument
#' fixed can still be passed more ellipsis arguments.
#'
#' \code{x_(1) $ xMap...(sqrt, 2, 3, 4, 5)}
#'
#' \bold{Anonymous Methods}
#'
#' Arrow provides a mechanism for applying normal R functions or anonymous functions
#' to data in an arrow container; the functions \bold{xTap( )} and \bold{x_Tap( )}.
#'
#' \code{x_(1:10) $ x_Tap(summary)}
#'
#' In this case the summary function is applied to the vector 1...10.
#'
#' @name help_arrow_methods

NULL
