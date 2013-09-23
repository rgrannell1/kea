\name{xFoldr}
\alias{xFoldr}
\title{Fold a function over a collection from right to length, with an init value.}
\usage{
  xFoldr(fn, init, coll)
}
\arguments{
  \item{fn}{a binary function that returns a value that
  \code{fn} can later take as its right argument}

  \item{init}{an arbitrary value.}

  \item{coll}{a collection.}
}
\value{
  an arbitrary value, depending on the function \code{fn}.
}
\description{
  Fold a function over a collection from right to length,
  with an init value.
}
\section{Corner Cases}{
  returns \code{init} if \code{coll} is length-zero.
}

