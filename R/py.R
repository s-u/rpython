py.init <- function(home=NULL, program=NULL) invisible(.Call(rpy_init, home, program))

py.get <- function(name, module=NULL, quiet=FALSE, .ref=FALSE) .Call(rpy_get, name, module, !quiet, .ref)

py.set <- function(name, value, module=NULL, quiet=FALSE) invisible(.Call(rpy_set, name, value, module, !quiet))

py.rcaps <- function(x) .Call(rpy_rcaps, x)

py.import <- function(module) invisible(.Call(rpy_import, module))

py.eval <- function(code, globals=NULL, locals=NULL, quiet=FALSE) .Call(rpy_eval, code, globals, locals, !quiet)

py.load <- function(source, quiet=FALSE) {
  x <- readLines(source)
  py.eval(paste(x, collapse="\n"), quiet=quiet)
}

py.call <- function(callable, ..., .ref=FALSE) .External(rpy_call, callable, ..., .ref = .ref)

## FIXME: this should not be needed since we're mapping exceptions, but jsut in case ...
py.exception <- function() .Call(rpy_fetch_ex)

py.attr <- function(x, name, .ref=FALSE) .Call(rpy_get_attr, x, name, .ref)

## temporarily until conditions contain the exception object...
py.last.exception <- function() .Call(rpy_last_ex)

as.character.pyref <- function(x, ...) .Call(rpy_as_string, x)

print.pyref <- function(x, ...) { cat("Python reference [", substr(as.character(x), 1, 80), "]\n", sep=''); invisible(x) }

as.pyref <- function(x) UseMethod("as.pyref")

as.pyref.pyref <- function(x) x

`$.pyref` <- function(x, name) .Call(rpy_get_attr, x, name, FALSE, FALSE)
`$<-.pyref` <- function(x, name, value) invisible(.Call(rpy_set_attr, x, name, value))
