\name{py.call}
\alias{py.call}
\title{Call a Python callable}
\description{
  \code{py.call} calls a Python callable - usually a function, built-in,
  method or a class object.
}
\usage{
py.call(callable, ..., .ref = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{callable}{Reference to a Python object that is callable}
  \item{\dots}{arguments to pass to the object}
  \item{.ref}{logical, if \code{TRUE} the result is a Python reference,
    otherwise conversion to a native R type is attempted.}
}
%\details{
%}
\value{
  Result of the call.
}
\author{
  Simon Urbanek
}
%\seealso{
%}
\examples{
py.init()

## create a simple class
py.eval("class foo:
   def __init__(self, value=0):
     self.x = value
   
   def f(self):
     self.x = self.x + 1
     return self.x")

## get the class object
foo = py.get("foo")
foo

## create an instance by calling it
i = py.call(foo)
i

i$f()
i$f()
i$x
i$x = 20
i$f()

## another instance with pre-set value
j = py.call(foo, 40)
j$x

}
\keyword{interface}
