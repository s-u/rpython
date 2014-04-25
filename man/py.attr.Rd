\name{py.attr}
\alias{py.attr}
\alias{$.pyref}
\alias{$<-.pyref}
\title{
  Get/set Python object attributes.
}
\description{
  \code{py.attr} retrieves the attribute of a Python object.

  \code{`$`} is a syntactic sugar for retrieving an attribute and
  \code{`$<-`} for setting it.
}
\usage{
py.attr(x, name, .ref = FALSE)
`$.pyref`(x, name)
`$<-.pyref`(x, name, value)
}
\arguments{
  \item{x}{Python reference object}
  \item{name}{name of the attribute}
  \item{value}{new value}
  \item{.ref}{logical, if \code{TRUE} a reference to the Python object
    is returned, otherwise a conversion to R native type is attempted}
}
\value{
}
%\references{
%}
\author{
  Simon Urbanek
}
\examples{
py.init()
main = py.import("__main__")
main$x = 1:10
py.eval("y = len(x)")
main$y
len = py.get("len", "__builtin__", .ref=TRUE)
len
len$`__call__`
}
\keyword{interface}
