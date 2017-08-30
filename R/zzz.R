.onLoad <- function(libname, pkgname) {
  repos = getOption("repos")
  repos["PaleogeoDB"] = "https://github.com/macroecology/PaleogeoDB"
  options(repos = repos)
  invisible(repos)
  
  install.github("macroecology/paleogeoDB")
}
