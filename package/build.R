#Roxygenize and build analogue package

# library(roxygen)

# roxygenize(package.dir="./analogues",
#              roxygen.dir="./analogues",
#              use.Rd2=TRUE,
#              overwrite=TRUE,
#              copy.package=FALSE,
#              unlink.target=FALSE)

#compiple the package for linux/unix
system(paste("R CMD build","analogues"))

#compile the package for windows
system(paste("R CMD build","--binary","analogues"))
