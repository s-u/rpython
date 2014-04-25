\name{py.import}
\alias{py.import}
\title{
  Import a Python module
}
\description{
  \code{py.import} import a module and return a reference to the
  module.
}
\usage{
py.import(module)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{module}{string, name of the module}
}
%\details{
%}
\value{
  Reference to the Python module. The \code{`$`} and \code{`$<-`}
  operators are supported to get/set values from/to the module -
  syntactic sugar for \code{\link{py.get}}, \code{\link{py.set}}.
}
%\references{
%}
\author{
  Simon Urbanek
}
%\seealso{
%}
\examples{
py.init()
main = py.import("__main__")
main$x = 1
py.eval("b = x + 1")
main$b
len = py.import("__builtin__")$len
len(1:10)
}
\keyword{interface}
