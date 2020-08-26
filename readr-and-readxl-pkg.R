library(dslabs)
library(tidyverse)
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format using readr format
filename <- "murders.csv"
dat <- read_csv(filename)

#read using full path
path <- system.file("extdata", package="dslabs")
fullpath <- file.path(path,filename)
dat <- read_csv(fullpath)
head(dat)

#Ex: using R-base import functions
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

# filename is defined in the previous video
# read.csv can converts strings to factors
dat3 <- read.csv(filename, stringsAsFactors = TRUE)
class(dat3$abb)
class(dat3$region)

#Downloading Files from the Internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

assesment
url2 <- "https://raw.githubusercontent.com/rasbt/python-machine-learning-book/master/code/datasets/wdbc/wdbc.data"
dat4 <- read_csv(url2, col_names = F)
head(dat4)
nrow(dat4)
ncol(dat4)
