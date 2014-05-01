## because of Python quirks on some platforms
## (it relies on flat namespace in its submodules - a really bad idea)
## we have to load it by hand into the global symbols space
## (again a really bad idea, but Phython relies on that,other
## wise none of its modules work since thay fail to link to Python)
.onLoad <- function(libname, pkgname) {
  .sym <- c('rpy_init', 'rpy_get', 'rpy_set', 'rpy_eval', 'rpy_import', 'rpy_as_string',
            'rpy_call', 'rpy_fetch_ex', 'rpy_last_ex', 'rpy_get_attr', 'rpy_set_attr')
  library.dynam(pkgname, pkgname, libname, local=FALSE, now=FALSE)
  ## now we have to register symbols by hand *sigh*
  e <- environment(.onLoad)
  si <- getNativeSymbolInfo(.sym, pkgname, FALSE)
  for (sym in .sym) e[[sym]] <- si[[sym]]$address
}
