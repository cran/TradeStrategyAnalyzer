.onLoad <- function(libname, pkgname) {
  .libPaths("~wwcheng/RLibrary")
  require(DBI)
  require(RSQLite)
  require(stats)
  require(RJSONIO)
  require(googleVis)
  require(ggplot2)
  require(digest)
}