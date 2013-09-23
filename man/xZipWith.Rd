\name{xZipWith}
\alias{xZipWith}
\title{Generate a list of n-element lists from n collections, and apply a function to each n-element list.}
\usage{
  xZipWith(fn, ...)
}
\arguments{
  \item{fn}{an n-ary function, or a symbol or name
  identifying such a function.}

  \item{...}{n-vectors, lists or pairlists.}
}
\value{
  returns a list of equal length to the shortest input
  collection.
}
\description{
  Generate a list of n-element lists from n collections,
  and apply a function to each n-element list.
}
\section{Corner Cases}{
  the empty list is returned if the shortest collection has
  length-zero, or no collections are included. Each
  collection is truncated to the length of the shortest
  collection.
}

