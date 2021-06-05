CallLibraries <- function(packages) {
  mapply(x = packages, MoreArgs = list(y = row.names(installed.packages())), function(x,y) {
    if (any(x %in% y)) {
      library(x, character.only = T)
    } else {
      install.packages(x)
      library(x, character.only = T)
      paste("installed:", x)
    }
    paste("called:", x)
  })
}