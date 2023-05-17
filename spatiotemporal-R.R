install.packages("devtools")
library(devtools)
install_github("andrewzm/STRbook")

install.packages(, type = 'source', repos = NULL)


install.packages("dplyr")
library("dplyr")
install.packages("tidyr")
library("tidyr")
install.packages("STRbook")
library("STRbook")

locs <- read.table(system.file("extdata", "Stationinfo.dat",
                               package = "STRbook"),
                   col.names = c("id", "lat", "lon"))
Times <- read.table(system.file("extdata", "Times_1990.dat",
                                package = "STRbook"),
                    col.names = c("julian", "year", "month", "day"))
Tmax <- read.table(system.file("extdata", "Tmax_1990.dat",
                               package = "STRbook"))
names(Tmax) <- locs$id
Tmax <- cbind(Times, Tmax)
head(names(Tmax), 10)
