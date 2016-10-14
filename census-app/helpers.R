install.packages(c("changepoint", "dplyr"))

#check_then_install <- function(pkg_name, pkg_version) {
#   if (!suppressWarnings(suppressMessages(require(pkg_name, character.only = TRUE)))) utils::install.packages(pkg_name, repos = "http://cran.r-project.org") else {
#   if (packageVersion(pkg_name) < package_version(pkg_version)) utils::install.packages(pkg_name, repos = "http://cran.r-project.org")
# }
#}

#check_then_install("changepoint")
#check_then_install("dplyr")
