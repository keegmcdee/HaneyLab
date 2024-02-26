#KJM Feb/25/2024
#Haney Lab
#Growth Curves Analysis 

install.packages("devtools")
library(devtools)
devtools::install_github("mikeblazanin/gcplyr")

##Introduction##
library(gcplyr)
library(dplyr)
library(ggplot2)

dat <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
##Import-Reshape##
#data is wide shaped 
head(dat)
#Take away temperature
dat_sorted <- select(dat, !Temperature..C.)

write.csv(dat_sorted, file="02_01GC.csv")

wide_data <- read_wides(files = "02_01GC.csv")         

#Specifying Metadata -> our files no metadata -> I could add?

#Transforming Data
#Tidy shaped is best for gcplyr 

colnames(wide_data)

tidy_data <- trans_wide_to_tidy(wides = wide_data,
                                data_cols = NA,
                                id_cols = c("file","Time")
  )

