\name{py.get}
\alias{py.get}
\alias{py.set}
\title{
  Get/set objects from a module
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
py.get(name, module = NULL, quiet = FALSE, .ref = FALSE)
py.set(name, value, module = NULL, quiet = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{object name}
  \item{value}{new value}
  \item{module}{module (reference or name) or \code{NULL} to use the
    \code{__main__} module}
  \item{quiet}{return \code{NULL} instead of an error if not present}
  \item{.ref}{logical, if \code{TRUE} a reference is returned, otherwise
    a conversion to a native type is attempted}
}
%\details{
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
  Simon Urbanek
}
%\seealso{
%}
\examples{
py.init()
py.eval("a=10")
py.get("a")
py.set("b",5)
py.get("len","__builtin__")(1:10)
}
\keyword{interface}
