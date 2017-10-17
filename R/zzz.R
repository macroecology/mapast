<<<<<<< HEAD
.onLoad <- function(libname, pkgname) {
=======
.onLoad <- function() {
  repos = getOption("repos")
  repos["PaleogeoDB"] = "https://github.com/macroecology/PaleogeoDB"
  options(repos = repos)
  invisible(repos)
>>>>>>> 050b84fc86fe6902de8ae7beaecbee00b14eaad4
  
  devtools::install_github("macroecology/paleogeoDB")

  invisible()
}
