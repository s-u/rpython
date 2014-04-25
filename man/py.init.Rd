\name{py.init}
\alias{py.init}
\title{
  Initialize Python interpreter
}
\description{
  \code{py.init} starts and intitializes the Python interpreter embedded
  in R.
}
\usage{
py.init(home = NULL, program = NULL)
}
\arguments{
  \item{home}{optional, string specifying the Python home. If not
    specified, Python typically uses \code{PYTONHOME} environment
    variable - see Python docuemntation for details.}
  \item{program}{optional, string specifying the name of the executable
    that Python should consider using in cases where it needs to start
    an external python interpreter.}
}
\details{
  This function must be called before using any other Python interface
  functions. Most function will called implicitly \code{py.init()} with
  no arguments and a warning if the interpreter has not been
  initialized.
}
\value{
  \code{TRUE} when successful, \code{FALSE} if the interpreter is has
  been already initialized before.
}
\author{
  Simon Urbanek
}
%\note{
%}
%\seealso{
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{interface}
