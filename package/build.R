#Roxygenize and build analogue package

library(roxygen)

roxygenize(package.dir="./analogues",
             roxygen.dir="./analogues",
             use.Rd2=TRUE,
             overwrite=TRUE,
             copy.package=FALSE,
             unlink.target=FALSE)

system(paste("R CMD build","analogues"))
system(paste("R CMD build","--binary","analogues"))
